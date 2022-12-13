/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.core.threadregistry;

import static org.junit.Assert.assertTrue;
import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.Tenant;
import de.hybris.platform.core.suspend.ResumeOptions;
import de.hybris.platform.core.suspend.SuspendOptions;
import de.hybris.platform.core.suspend.SuspendResult;
import de.hybris.platform.core.suspend.SystemStatus;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.tx.AfterSaveEvent;
import de.hybris.platform.tx.AfterSaveEventChangesCollector;
import de.hybris.platform.tx.AfterSaveListener;
import de.hybris.platform.tx.DefaultAfterSaveListenerRegistry;

import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mockito;

@IntegrationTest
public class NotSuspendableAfterSaveEventTest extends ServicelayerBaseTest
{

	@BeforeClass
	public static void startMasterTenant()
	{
		//Suspend resume is a feature which is not tenant aware. It logic depends on master tenant being active.
		final Tenant previousTenant = Registry.activateMasterTenant();
		if (previousTenant == null)
		{
			Registry.unsetCurrentTenant();
		}
		else
		{
			Registry.setCurrentTenant(previousTenant);
		}
	}

	final PK pk1 = PK.createFixedCounterPK(102, 1);

	final AfterSaveEventChangesCollector eventCollector = new AfterSaveEventChangesCollector();

	@Test
	public void shouldNotifyListenersAsNotSuspendableOperation() throws Exception
	{
		// given
		final Set<Long> threadIds = getThreadsIdForAfterSaveEventPublisher();
		final DefaultAfterSaveListenerRegistry registry = getAfterSaveListenerRegistryWithAsyncSending(1000);
		registry.afterPropertiesSet();
		try
		{
			final Set<Long> threadIdsWithNewAfterSaveEventPublisherThread = getThreadsIdForAfterSaveEventPublisher();
			threadIdsWithNewAfterSaveEventPublisherThread.removeAll(threadIds);

			final TestAfterSaveListener listener = new TestAfterSaveListener();
			registry.addListener(listener);
			// when
			registry.publishChanges(generateChange(pk1));

			final Long threadToSearch = threadIdsWithNewAfterSaveEventPublisherThread.iterator().next();
			// then
			assertTrue("listener not finished in time", listener.waitForFirstEventPassed(30, TimeUnit.SECONDS));
			assertThat(getOperationInfoForThreadId(threadToSearch).get().canBeSuspended()).isFalse();
			Thread.sleep(2000);
			assertThat(getOperationInfoForThreadId(threadToSearch).get().canBeSuspended()).isTrue();
		}
		finally
		{
			registry.destroy();
		}
	}

	@Test
	public void shouldNotifyListenersAfterSystemResume() throws Exception
	{
		final Set<Long> threadIds = getThreadsIdForAfterSaveEventPublisher();
		final DefaultAfterSaveListenerRegistry registry = getAfterSaveListenerRegistryWithAsyncSending(1000);
		registry.afterPropertiesSet();
		try
		{
			final Set<Long> threadIdsWithNewAfterSaveEventPublisherThread = getThreadsIdForAfterSaveEventPublisher();
			threadIdsWithNewAfterSaveEventPublisherThread.removeAll(threadIds);

			final TestAfterSaveListener listener = Mockito.spy(new TestAfterSaveListener());
			registry.addListener(listener);

			final DefaultSuspendResumeService service = givenSuspendResumeService(defaultThreadRegistry());

			assertThat(getOperationInfoForThreadId(threadIdsWithNewAfterSaveEventPublisherThread.iterator().next()).get()
			                                                                                                       .canBeSuspended())
					.isTrue();

			final SuspendResult suspendResult = service.suspend(SuspendOptions.defaultOptions());
			registry.publishChanges(generateChange(pk1));

			Thread.sleep(1000);

			Mockito.verify(listener, Mockito.times(0)).afterSave(Mockito.anyCollection());
			service.resume(ResumeOptions.builder().withResumeToken(suspendResult.getResumeToken()).build());

			assertThat(service.getSystemStatus()).isSameAs(SystemStatus.RUNNING);
			assertTrue("listener not finished in time", listener.waitForFirstEventPassed(30, TimeUnit.SECONDS));
		}
		finally
		{
			registry.destroy();
		}
	}

	private DefaultAfterSaveListenerRegistry getAfterSaveListenerRegistryWithAsyncSending(final int activeTime)
	{
		final DefaultAfterSaveListenerRegistry registry = new DefaultAfterSaveListenerRegistry()
		{
			@Override
			protected boolean isAsyncParam()
			{
				return true;
			}

			@Override
			protected long getActiveTimeParam()
			{
				return activeTime;
			}
		};
		return registry;
	}

	private Optional<OperationInfo> getOperationInfoForThreadId(final Long parentId)
	{
		return defaultThreadRegistry().getAllOperations()
		                              .entrySet()
		                              .stream()
		                              .filter(e -> e.getKey().equals(parentId))
		                              .map(Map.Entry::getValue)
		                              .findFirst();


	}

	private Set<Long> getThreadsIdForAfterSaveEventPublisher()
	{
		return defaultThreadRegistry().getAllOperations()
		                              .entrySet()
		                              .stream()
		                              .filter(e -> ((String) e.getValue()
		                                                      .getAttribute(
				                                                      OperationInfo.StandardAttributes.THREAD_NAME)).startsWith(
				                              "AfterSaveEventPublisher-" + Registry.getSlaveJunitTenant().getTenantID()))
		                              .map(e -> (Long) e.getValue().getAttribute(OperationInfo.StandardAttributes.THREAD_ID))
		                              .collect(
				                              Collectors.toSet());
	}

	private byte[][] generateChange(final PK pk)
	{
		eventCollector.clear();
		eventCollector.collect(pk, AfterSaveEvent.CREATE);
		return eventCollector.getEncodedChanges();
	}

	public static class TestAfterSaveListener implements AfterSaveListener
	{
		final CountDownLatch firstInvocationPassed = new CountDownLatch(1);

		final Collection<AfterSaveEvent> events = new CopyOnWriteArrayList<>();
		volatile boolean firstTime = true;

		@Override
		public void afterSave(final Collection<AfterSaveEvent> events)
		{
			this.events.addAll(events);

			if (firstTime)
			{
				firstTime = false;
				doOnFirstInvocation();
			}
		}

		public void doOnFirstInvocation()
		{
			firstInvocationPassed.countDown();
		}

		boolean waitForFirstEventPassed(final long timeout, final TimeUnit unit) throws InterruptedException
		{
			return firstInvocationPassed.await(timeout, unit);
		}
	}

	private ThreadRegistry defaultThreadRegistry()
	{
		return SuspendResumeServices.getInstance().getThreadRegistry();
	}

	private DefaultSuspendResumeService givenSuspendResumeService(final ThreadRegistry threadRegistry)
	{
		return new DefaultSuspendResumeService(threadRegistry);
	}

}
