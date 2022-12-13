/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.cronjob;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.cluster.ClusterNodeInfo;
import de.hybris.platform.cluster.DefaultClusterNodeManagementService;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.Tenant;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.MockitoAnnotations;

@IntegrationTest
public class StaleCronJobUnlockerTest extends ServicelayerBaseTest
{

	private final PropertyConfigSwitcher propertyActive = new PropertyConfigSwitcher(
			StaleCronJobUnlocker.PROPERTY_CRONJOB_UNLOCKER_ACTIVE);
	private final PropertyConfigSwitcher propertyInterval = new PropertyConfigSwitcher(
			StaleCronJobUnlocker.PROPERTY_CRONJOB_UNLOCKER_INTERVAL);
	private final PropertyConfigSwitcher propertyNodeTimeout = new PropertyConfigSwitcher(
			StaleCronJobUnlocker.PROPERTY_CRONJOB_STALE_NODE_INTERVAL);
	private final PropertyConfigSwitcher propertyNodeTimeoutCutoffInterval = new PropertyConfigSwitcher(
			StaleCronJobUnlocker.PROPERTY_CRONJOB_STALE_NODE_CUTOFF_INTERVAL);


	private final Duration defaultThreadInterval = Duration.ofSeconds(2);
	AtomicInteger mockClusterNodeIdCounter = new AtomicInteger(100);
	private StaleCronJobUnlocker thread;

	@Captor
	private ArgumentCaptor<Collection<Integer>> nodeIdsCaptor;

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);

		propertyActive.switchToValue("false");
		propertyInterval.switchToValue("1000");
		propertyNodeTimeout.switchToValue("2000");
		propertyNodeTimeoutCutoffInterval.switchToValue("100");

	}

	@After
	public void tearDown() throws Exception
	{
		propertyActive.switchBackToDefault();
		propertyInterval.switchBackToDefault();
		propertyNodeTimeout.switchBackToDefault();
		propertyNodeTimeoutCutoffInterval.switchBackToDefault();

		if (thread != null && thread.isAlive())
		{
			thread.interrupt();
		}
	}

	@Test
	public void shouldUseAllRegisteredNodesToUnlockCronJobs() throws InterruptedException
	{
		final ClusterNodeInfo node1 = mockMockClusterNodeInfo();
		final ClusterNodeInfo node2 = mockMockClusterNodeInfo();
		final ClusterNodeInfo node3 = mockMockClusterNodeInfo();
		final ClusterNodeInfo staleNode1 = mockMockClusterNodeInfo(Instant.now().minusSeconds(50));
		final ClusterNodeInfo oldNode1 = mockMockClusterNodeInfo(Instant.now().minusSeconds(500));


		thread = spy(TestStaleCronJobUnlocker.forTenant(Registry.getCurrentTenant())
		                                     .withGetAllNodesSupplier(() -> Set.of(node1, node2, node3, staleNode1, oldNode1))
		                                     .withUnlockCronJobsForNodeIdsConsumer((staleNodeIds, allNodeIds) -> {
		                                     })
		                                     .build());

		propertyActive.switchToValue("true");

		thread.start();
		Thread.sleep(defaultThreadInterval.toMillis());
		thread.stopUpdatingAndFinish(defaultThreadInterval.toMillis());


		verify(thread, atLeastOnce()).unlockCronJobsForNodeIds(any(), nodeIdsCaptor.capture());

		assertThat(nodeIdsCaptor.getAllValues()).isNotEmpty();
		for (final Collection<Integer> allNodeIds : nodeIdsCaptor.getAllValues())
		{
			assertThat(allNodeIds).containsExactlyInAnyOrder(node1.getId(), node2.getId(), node3.getId(), staleNode1.getId(),
					oldNode1.getId());
		}
	}

	@Test
	public void shouldUseStaleNodesToUnlockCronJobs() throws InterruptedException
	{

		final ClusterNodeInfo node1 = mockMockClusterNodeInfo();
		final ClusterNodeInfo node2 = mockMockClusterNodeInfo();
		final ClusterNodeInfo node3 = mockMockClusterNodeInfo();
		final ClusterNodeInfo staleNode1 = mockMockClusterNodeInfo(Instant.now().minusSeconds(50));
		final ClusterNodeInfo oldNode1 = mockMockClusterNodeInfo(Instant.now().minusSeconds(500));

		thread = spy(TestStaleCronJobUnlocker.forTenant(Registry.getCurrentTenant())
		                                     .withGetAllNodesSupplier(() -> Set.of(node1, node2, node3, staleNode1, oldNode1))
		                                     .build());

		propertyActive.switchToValue("true");
		propertyNodeTimeout.switchToValue("20000");
		thread.start();
		Thread.sleep(defaultThreadInterval.toMillis());
		thread.stopUpdatingAndFinish(defaultThreadInterval.toMillis());


		verify(thread, atLeastOnce()).unlockCronJobsForNodeIds(nodeIdsCaptor.capture(), any());

		assertThat(nodeIdsCaptor.getAllValues()).isNotEmpty();
		for (final Collection<Integer> staleNodeIds : nodeIdsCaptor.getAllValues())
		{
			assertThat(staleNodeIds).containsExactlyInAnyOrder(staleNode1.getId());
		}
	}

	@Test
	public void shouldRunUnlockingOfLocalInstanceCronJobs() throws InterruptedException
	{
		thread = spy(TestStaleCronJobUnlocker.forTenant(Registry.getCurrentTenant()).build());

		propertyActive.switchToValue("true");

		thread.start();
		Thread.sleep(defaultThreadInterval.toMillis());
		thread.stopUpdatingAndFinish(defaultThreadInterval.toMillis());

		verify(thread, atLeastOnce()).unlockCronJobsFromLocalInstance();
	}

	private ClusterNodeInfo mockMockClusterNodeInfo()
	{
		return mockMockClusterNodeInfo(Instant.now());
	}

	private ClusterNodeInfo mockMockClusterNodeInfo(final Instant lastPing)
	{
		return new ClusterNodeInfo(mockClusterNodeIdCounter.incrementAndGet(), "testNode-" + UUID.randomUUID().toString(),
				lastPing.minusSeconds(150).toEpochMilli(), lastPing.toEpochMilli());
	}

	@Test
	public void shouldBeActiveWhenPropertyIsTrue() throws InterruptedException
	{
		thread = spy(TestStaleCronJobUnlocker.forTenant(Registry.getCurrentTenant()).build());

		propertyActive.switchToValue("true");

		thread.start();
		Thread.sleep(defaultThreadInterval.toMillis());
		thread.stopUpdatingAndFinish(defaultThreadInterval.toMillis());

		verify(thread, atLeastOnce()).getStaleNodeIds(any(), any());
	}

	@Test
	public void shouldBeInactiveWhenPropertyIsFalse() throws InterruptedException
	{
		thread = spy(TestStaleCronJobUnlocker.forTenant(Registry.getCurrentTenant()).build());

		propertyActive.switchToValue("false");

		thread.start();
		Thread.sleep(defaultThreadInterval.toMillis());
		thread.stopUpdatingAndFinish(defaultThreadInterval.toMillis());

		verify(thread, never()).getStaleNodeIds(any(), any());
	}

	@Test
	public void shouldUseProvidedInterval() throws InterruptedException
	{
		final ExecutionIntervalMarker marker = new ExecutionIntervalMarker();
		thread = spy(TestStaleCronJobUnlocker.forTenant(Registry.getCurrentTenant())
		                                     .withUnlockCronJobsForNodeIdsConsumer((staleNodes, allNodes) -> marker.mark())
		                                     .build());

		propertyActive.switchToValue("true");
		propertyInterval.switchToValue("1000");

		thread.start();
		Thread.sleep(defaultThreadInterval.multipliedBy(2).toMillis());

		propertyInterval.switchToValue("1500");
		Thread.sleep(defaultThreadInterval.multipliedBy(2).toMillis());

		thread.stopUpdatingAndFinish(defaultThreadInterval.toMillis());

		verify(thread, atLeastOnce()).getStaleNodeIds(any(), any());

		final List<Duration> intervals = marker.getIntervals();
		assertThat(intervals).isNotEmpty();
		System.out.println(intervals);
		final Map<Long, Long> collect = intervals.stream()
		                                         .collect(Collectors.groupingBy(
				                                         duration -> Math.round(duration.toMillis() / 500.0) * 500,
				                                         Collectors.counting()));
		assertThat(collect.keySet()).containsExactlyInAnyOrder(1000L, 1500L);
	}

	@Test
	public void shouldReturnValidStaleNodeThresholdInterval()
	{
		thread = spy(TestStaleCronJobUnlocker.forTenant(Registry.getCurrentTenant()).build());

		assertThat(thread.getStaleNodeThresholdInterval()).isEqualByComparingTo(Duration.ofMillis(2000));

		propertyNodeTimeout.switchToValue("5000");
		assertThat(thread.getStaleNodeThresholdInterval()).isEqualByComparingTo(Duration.ofMillis(5000));

		propertyNodeTimeout.switchToValue("100");
		assertThat(thread.getStaleNodeThresholdInterval()).isEqualByComparingTo(Duration.ofMillis(100));

		propertyNodeTimeout.switchToValue("0");
		assertThat(thread.getStaleNodeThresholdInterval()).isEqualByComparingTo(Duration.ofMillis(
				DefaultClusterNodeManagementService.getInstance().getStaleNodeTimeout()));

		propertyNodeTimeout.switchToValue("-100");
		assertThat(thread.getStaleNodeThresholdInterval()).isEqualByComparingTo(Duration.ofMillis(
				DefaultClusterNodeManagementService.getInstance().getStaleNodeTimeout()));
	}

	@Test
	public void shouldCalculateValidStaleNodeThreshold()
	{
		thread = spy(TestStaleCronJobUnlocker.forTenant(Registry.getCurrentTenant()).build());

		final Duration staleNodeThresholdInterval = thread.getStaleNodeThresholdInterval();
		assertThat(staleNodeThresholdInterval).isEqualByComparingTo(Duration.ofMillis(2000));

		final Instant now = Instant.now();

		final Instant staleNodeTsThreshold = thread.getStaleNodeTsThreshold(now, staleNodeThresholdInterval);

		assertThat(staleNodeTsThreshold).isEqualByComparingTo(now.minus(staleNodeThresholdInterval));
	}


	@Test
	public void shouldCalculateValidStaleNodeCutoffWhenValueIsGreaterThanZero()
	{
		thread = spy(TestStaleCronJobUnlocker.forTenant(Registry.getCurrentTenant()).build());

		propertyNodeTimeoutCutoffInterval.switchToValue("20000");

		final Instant now = Instant.now();

		final Instant staleNodeTsCutoff = thread.getStaleNodeTsCutoff(now);
		assertThat(staleNodeTsCutoff).isLessThan(now);

		final Instant expectedCutoffTs = now.minus(20000, ChronoUnit.SECONDS);
		assertThat(staleNodeTsCutoff).isEqualByComparingTo(expectedCutoffTs);
	}

	@Test
	public void shouldCalculateValidStaleNodeCutoffWhenValueIsZero()
	{
		thread = spy(TestStaleCronJobUnlocker.forTenant(Registry.getCurrentTenant()).build());

		propertyNodeTimeoutCutoffInterval.switchToValue("0");

		final Instant now = Instant.now();

		final Instant staleNodeTsCutoff = thread.getStaleNodeTsCutoff(now);

		assertThat(staleNodeTsCutoff).isEqualByComparingTo(Instant.EPOCH);
	}

	@Test
	public void shouldCalculateValidStaleNodeCutoffWhenValueIsLesserThanZero()
	{
		thread = spy(TestStaleCronJobUnlocker.forTenant(Registry.getCurrentTenant()).build());

		propertyNodeTimeoutCutoffInterval.switchToValue("-1");

		final Instant now = Instant.now();

		final Instant staleNodeTsCutoff = thread.getStaleNodeTsCutoff(now);

		assertThat(staleNodeTsCutoff).isEqualByComparingTo(Instant.EPOCH);
	}


	@Test
	public void shouldRunEvenWithExceptionsInGetAllNodes() throws InterruptedException
	{
		thread = spy(TestStaleCronJobUnlocker.forCurrentTenant().withGetAllNodesSupplier(() -> {
			throw new RuntimeException("something went horribly wrong");
		}).build());

		propertyActive.switchToValue("true");

		thread.start();
		Thread.sleep(defaultThreadInterval.multipliedBy(2).toMillis());
		thread.stopUpdatingAndFinish(defaultThreadInterval.toMillis());

		verify(thread, atLeast(2)).getStaleNodeThresholdInterval();
	}

	@Test
	public void shouldRunEvenWithExceptionsInGetNodesIds() throws InterruptedException
	{
		thread = spy(TestStaleCronJobUnlocker.forCurrentTenant().withGetNodesIdsFunction(infos -> {
			throw new RuntimeException("something went horribly wrong");
		}).build());

		propertyActive.switchToValue("true");

		thread.start();
		Thread.sleep(defaultThreadInterval.multipliedBy(2).toMillis());
		thread.stopUpdatingAndFinish(defaultThreadInterval.toMillis());

		verify(thread, atLeast(2)).getStaleNodeThresholdInterval();
	}


	@Test
	public void shouldRunEvenWithExceptionsInUnlockCronJobsForNodeIds() throws InterruptedException
	{
		thread = spy(TestStaleCronJobUnlocker.forCurrentTenant().withUnlockCronJobsForNodeIdsConsumer((staleIds, allIds) -> {
			throw new RuntimeException("something went horribly wrong");
		}).build());

		propertyActive.switchToValue("true");

		thread.start();
		Thread.sleep(defaultThreadInterval.multipliedBy(2).toMillis());
		thread.stopUpdatingAndFinish(defaultThreadInterval.toMillis());

		verify(thread, atLeast(2)).getStaleNodeThresholdInterval();
	}

	@Test
	public void shouldRunEvenWithExceptionsInGetStaleNodesIds() throws InterruptedException
	{
		thread = spy(TestStaleCronJobUnlocker.forCurrentTenant().withGetStaleNodesIdsFunction((ids, predicate) -> {
			throw new RuntimeException("something went horribly wrong");
		}).build());

		propertyActive.switchToValue("true");

		thread.start();
		Thread.sleep(defaultThreadInterval.multipliedBy(2).toMillis());
		thread.stopUpdatingAndFinish(defaultThreadInterval.toMillis());

		verify(thread, atLeast(2)).getStaleNodeThresholdInterval();
	}

	@Test
	public void shouldRunEvenWithExceptionsInUnlockCronJobsFromLocalInstance() throws InterruptedException
	{
		thread = spy(TestStaleCronJobUnlocker.forCurrentTenant().withUnlockCronJobsFromLocalInstanceRunnable(() -> {
			throw new RuntimeException("something went horribly wrong");
		}).build());

		propertyActive.switchToValue("true");

		thread.start();
		Thread.sleep(defaultThreadInterval.multipliedBy(2).toMillis());
		thread.stopUpdatingAndFinish(defaultThreadInterval.toMillis());

		verify(thread, atLeast(2)).getStaleNodeThresholdInterval();
	}

	public static class TestStaleCronJobUnlocker extends StaleCronJobUnlocker
	{
		private final BiFunction<Collection<ClusterNodeInfo>, Predicate<ClusterNodeInfo>, List<Integer>> getStaleNodeIdsFunction;
		private final Function<Collection<ClusterNodeInfo>, List<Integer>> getNodeIdsFunction;
		private final BiConsumer<Collection<Integer>, Collection<Integer>> unlockCronJobsForNodeIdsConsumer;
		private final Supplier<Collection<ClusterNodeInfo>> getAllNodesSupplier;
		private final Runnable unlockCronJobsFromLocalInstanceRunnable;

		private TestStaleCronJobUnlocker(final Builder builder)
		{
			super("testing", builder.tenant);
			//this(tenant, (clusterNodeInfoPredicate, clusterNodeInfos) -> Collections.emptyList());
			this.getStaleNodeIdsFunction = builder.getStaleNodesIdsFunction;
			this.getNodeIdsFunction = builder.getNodesIdsFunction;
			this.unlockCronJobsForNodeIdsConsumer = builder.unlockCronJobsForNodeIdsConsumer;
			this.getAllNodesSupplier = builder.getAllNodesSupplier;
			this.unlockCronJobsFromLocalInstanceRunnable = builder.unlockCronJobsFromLocalInstanceRunnable;
		}

		public static Builder forCurrentTenant()
		{
			return forTenant(Registry.getCurrentTenant());
		}

		public static Builder forTenant(final Tenant tenant)
		{
			return new Builder(tenant);
		}

		@Override
		protected List<Integer> getStaleNodeIds(final Collection<ClusterNodeInfo> allNodes,
		                                        final Predicate<ClusterNodeInfo> staleNodePredicate)
		{
			if (getStaleNodeIdsFunction == null)
			{
				return super.getStaleNodeIds(allNodes, staleNodePredicate);
			}
			return getStaleNodeIdsFunction.apply(allNodes, staleNodePredicate);
		}

		@Override
		protected void unlockCronJobsForNodeIds(final Collection<Integer> staleNodes, final Collection<Integer> allNodes)
		{
			if (unlockCronJobsForNodeIdsConsumer == null)
			{
				super.unlockCronJobsForNodeIds(staleNodes, allNodes);
			}
			else
			{
				unlockCronJobsForNodeIdsConsumer.accept(staleNodes, allNodes);
			}
		}

		@Override
		protected void unlockCronJobsFromLocalInstance()
		{
			if (unlockCronJobsFromLocalInstanceRunnable == null)
			{
				super.unlockCronJobsFromLocalInstance();
			}
			else
			{
				unlockCronJobsFromLocalInstanceRunnable.run();
			}
		}

		@Override
		protected List<Integer> getNodeIds(final Collection<ClusterNodeInfo> nodes)
		{
			if (getNodeIdsFunction == null)
			{
				return super.getNodeIds(nodes);
			}
			return getNodeIdsFunction.apply(nodes);
		}

		@Override
		protected Collection<ClusterNodeInfo> getAllNodes()
		{
			if (getAllNodesSupplier == null)
			{
				return super.getAllNodes();
			}
			return getAllNodesSupplier.get();
		}

		public static class Builder
		{

			private Tenant tenant;
			private Supplier<Collection<ClusterNodeInfo>> getAllNodesSupplier = null;
			private BiFunction<Collection<ClusterNodeInfo>, Predicate<ClusterNodeInfo>, List<Integer>> getStaleNodesIdsFunction = null;
			private Function<Collection<ClusterNodeInfo>, List<Integer>> getNodesIdsFunction = null;
			private Runnable unlockCronJobsFromLocalInstanceRunnable = null;
			//by default do not call the real method
			private BiConsumer<Collection<Integer>, Collection<Integer>> unlockCronJobsForNodeIdsConsumer = (a, b) -> {
			};

			private Builder(final Tenant tenant)
			{
				this.tenant = tenant;
			}

			public Builder withTenant(final Tenant tenant)
			{
				this.tenant = tenant;
				return this;
			}

			public Builder withGetStaleNodesIdsFunction(
					final BiFunction<Collection<ClusterNodeInfo>, Predicate<ClusterNodeInfo>, List<Integer>> f)
			{
				getStaleNodesIdsFunction = f;
				return this;
			}

			public Builder withGetNodesIdsFunction(
					final Function<Collection<ClusterNodeInfo>, List<Integer>> getNodesIdsFunction)
			{
				this.getNodesIdsFunction = getNodesIdsFunction;
				return this;
			}

			public Builder withUnlockCronJobsForNodeIdsConsumer(
					final BiConsumer<Collection<Integer>, Collection<Integer>> unlockCronJobsForNodeIdsConsumer)
			{
				this.unlockCronJobsForNodeIdsConsumer = unlockCronJobsForNodeIdsConsumer;
				return this;
			}

			public Builder withGetAllNodesSupplier(final Supplier<Collection<ClusterNodeInfo>> getAllNodesSupplier)
			{
				this.getAllNodesSupplier = getAllNodesSupplier;
				return this;
			}

			public Builder withUnlockCronJobsFromLocalInstanceRunnable(final Runnable unlockCronJobsFromLocalInstanceRunnable)
			{
				this.unlockCronJobsFromLocalInstanceRunnable = unlockCronJobsFromLocalInstanceRunnable;
				return this;
			}

			public TestStaleCronJobUnlocker build()
			{
				return new TestStaleCronJobUnlocker(this);
			}
		}
	}

	private static class ExecutionIntervalMarker
	{

		List<Instant> executionInstants = new ArrayList<>();
		List<Duration> intervals = new ArrayList<>();
		AtomicReference<Instant> lastExecution = new AtomicReference<>();

		public void mark()
		{
			final Instant now = Instant.now();
			final Instant prev = lastExecution.getAndSet(now);

			executionInstants.add(now);
			if (prev != null)
			{
				intervals.add(Duration.between(prev, now));
			}
		}

		public List<Duration> getIntervals()
		{
			return intervals;
		}
	}
}
