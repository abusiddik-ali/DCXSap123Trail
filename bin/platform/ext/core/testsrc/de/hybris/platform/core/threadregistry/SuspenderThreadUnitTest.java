/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core.threadregistry;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.suspend.SuspendOptions;

import de.hybris.platform.regioncache.test.helper.ThreadDump;

import org.junit.Test;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CountDownLatch;


@UnitTest
public class SuspenderThreadUnitTest
{

	@Test
	public void shouldBeNamedSuspenderThread()
	{
		final ThreadRegistry tr = givenThreadRegistry();
		final SuspenderThread thread = givenSuspenderThread(tr);

		assertThat(thread.getName()).isEqualTo(SuspenderThread.class.getSimpleName());
	}

	@Test
	public void shouldRegisterItselfInTheRegistryAndUnregisterAtTheEnd() throws InterruptedException
	{
		final ThreadRegistry tr = givenThreadRegistry();
		final SuspenderThread thread = givenSuspenderThread(tr);
		final Long threadId = Long.valueOf(thread.getId());

		thread.startAndWaitForThreadToBeRunning();
		assertThat(tr.getAllOperations()).containsOnlyKeys(threadId, loggerThreadId(tr.getAllOperations()));

		thread.join();
		Thread.sleep(300);

		assertThat(tr.getAllOperations()).isEmpty();
	}

	@Test
	public void shouldFinishRightAfterThereIsNoNotSuspendableThreads() throws InterruptedException
	{
		final ThreadRegistry tr = givenThreadRegistry();
		final SuspenderThread thread = givenSuspenderThread(tr);
		final Long threadId = Long.valueOf(thread.getId());


		RegistrableThread.registerThread(OperationInfo.builder().asNotSuspendableOperation().build(), tr);

		thread.startAndWaitForThreadToBeRunning();

		assertThat(tr.getAllOperations()).containsOnlyKeys(threadId, loggerThreadId(tr.getAllOperations()),
				Long.valueOf(Thread.currentThread().getId()));

		thread.join(1000);
		assertThat(thread.isAlive()).isTrue();

		RegistrableThread.unregisterThread(tr);
		thread.join(1000);

		Thread.sleep(300);

		assertThat(tr.getAllOperations()).isEmpty();
	}

	@Test
	public void shouldStopOnDemand() throws InterruptedException
	{
		final ThreadRegistry tr = givenThreadRegistry();
		final SuspenderThread thread = givenSuspenderThread(tr);
		final Long threadId = Long.valueOf(thread.getId());

		RegistrableThread.registerThread(OperationInfo.builder().asNotSuspendableOperation().build(), tr);

		thread.startAndWaitForThreadToBeRunning();
		assertThat(tr.getAllOperations()).containsOnlyKeys(threadId, loggerThreadId(tr.getAllOperations()),
				Long.valueOf(Thread.currentThread().getId()));

		thread.stopAndWaitForThreadToBeFinished();

		assertThat(thread.isAlive()).isFalse();
		assertThat(tr.getAllOperations()).doesNotContainKey(threadId);
	}

	@Test
	public void shouldStopWhenInterrupted() throws InterruptedException
	{
		final ThreadRegistry tr = givenThreadRegistry();
		final SuspenderThread thread = givenSuspenderThread(tr);
		final Long threadId = Long.valueOf(thread.getId());

		RegistrableThread.registerThread(OperationInfo.builder().asNotSuspendableOperation().build(), tr);

		thread.startAndWaitForThreadToBeRunning();
		assertThat(tr.getAllOperations()).containsOnlyKeys(threadId, loggerThreadId(tr.getAllOperations()),
				Long.valueOf(Thread.currentThread().getId()));

		thread.join(1000);
		assertThat(thread.isAlive()).isTrue();

		thread.interrupt();

		thread.join(1000);
		assertThat(thread.isAlive()).isFalse();
		assertThat(tr.getAllOperations()).doesNotContainKey(threadId);
	}

	private SuspenderThread givenSuspenderThread(final ThreadRegistry threadRegistry)
	{
		final SuspenderThread thread = new SuspenderThread(SuspendOptions.defaultOptions(), threadRegistry);
		thread.setWaitTime(100);
		return thread;
	}

	private ThreadRegistry givenThreadRegistry()
	{
		return new ThreadRegistry(() -> false);
	}

	private long loggerThreadId(final Map<Long, OperationInfo> operations)
	{
		final String loggerThreadOperationName = SuspendResumeLoggerThread.class.getSimpleName();
		final Optional<OperationInfo> loggerThreadOperationInfo = operations.values().stream()
		                                                                    .filter(info -> info.getAttribute(
				                                                                    OperationInfo.StandardAttributes.THREAD_NAME)
		                                                                                        .equals(loggerThreadOperationName))
		                                                                    .findAny();
		if (loggerThreadOperationInfo.isPresent())
		{
			return loggerThreadOperationInfo.get().getAttribute(OperationInfo.StandardAttributes.THREAD_ID);
		}

		return -1;
	}

}
