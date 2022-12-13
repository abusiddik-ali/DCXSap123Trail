/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.masterserver.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.withinPercentage;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.threadregistry.RegistrableThread;
import de.hybris.platform.util.backoff.BackoffStrategy;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.OptionalDouble;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.google.common.base.Stopwatch;

@UnitTest
public class StatisticsPublisherWorkerTest
{
	@Mock
	private BackoffStrategy backoffStrategy;

	@Mock
	private StatisticsPublisher.StatisticsSender statisticsSender;

	private ExecutorService executor;

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);
	}


	@Test(expected = NullPointerException.class)
	public void shouldThrowExceptionWhenBackOffStrategyIsNotProvided()
	{
		final StatisticsPublisher.StatisticsPublisherWorker worker = new StatisticsPublisher.StatisticsPublisherWorker(
				statisticsSender, null, Duration.ofMillis(10L));
	}

	@Test(expected = NullPointerException.class)
	public void shouldThrowExceptionWhenStatisticsSenderIsNotProvided()
	{
		final StatisticsPublisher.StatisticsPublisherWorker worker = new StatisticsPublisher.StatisticsPublisherWorker(
				null, backoffStrategy, Duration.ofMillis(10L));
	}

	@Test(expected = NullPointerException.class)
	public void shouldThrowExceptionWhenSendingIntervalIsNotProvided()
	{
		final StatisticsPublisher.StatisticsPublisherWorker worker = new StatisticsPublisher.StatisticsPublisherWorker(
				statisticsSender, backoffStrategy, null);
	}

	@Test
	public void shouldSuspendThreadWorkAsExpectedWithNoTimeLeftToWait()
	{
		final Duration expectedDuration = Duration.ofSeconds(3);
		final TestWorker testWorker = getTestWorker();

		final Duration duration = measureWaitTime(() -> testWorker.suspendThread(expectedDuration));
		assertThat(duration.toMillis()).isCloseTo(expectedDuration.toMillis(), withinPercentage(5));
	}

	@Test
	public void shouldSuspendThreadWorkAsExpectedWithTimeLeftToWait()
	{
		final Duration expectedDuration = Duration.ofMillis(3900);
		final TestWorker testWorker = getTestWorker();

		final Duration duration = measureWaitTime(() -> testWorker.suspendThread(expectedDuration));
		assertThat(duration.toMillis()).isCloseTo(expectedDuration.toMillis(), withinPercentage(5));
	}

	@Test
	public void shouldSuspendThreadWorkAsExpectedWithDurationLessThanTimeLeftToWait()
	{
		final Duration expectedDuration = Duration.ofMillis(400);
		final TestWorker testWorker = getTestWorker();

		final Duration duration = measureWaitTime(() -> testWorker.suspendThread(expectedDuration));
		assertThat(duration.toMillis()).isCloseTo(expectedDuration.toMillis(), withinPercentage(20));
	}

	@Test
	public void shouldSuspendThreadTakeNoActionWhenShouldThreadWorkIsFalse()
	{
		final Duration expectedDuration = Duration.ofMillis(3900);
		final TestWorker testWorker = Mockito.spy(getTestWorker());

		when(testWorker.shouldThreadWork()).thenReturn(false);
		final Duration duration = measureWaitTime(() -> testWorker.suspendThread(expectedDuration));

		assertThat(duration).isBetween(Duration.ofMillis(0), Duration.ofMillis(50));
	}

	private Duration measureWaitTime(final Runnable runnable)
	{
		final Stopwatch stopwatch = Stopwatch.createStarted();
		runnable.run();
		return stopwatch.elapsed();
	}

	@Test
	public void shouldSendStatisticsWhenBackOffStrategyAllows()
	{
		when(backoffStrategy.shouldRetry()).thenReturn(true);
		when(statisticsSender.sendStatistics(any())).thenReturn(true);

		final StatisticsPublisher.StatisticsPublisherWorker worker = getTestWorker();

		executor = Executors.newSingleThreadExecutor(RegistrableThread::new);
		final Future<?> t = executor.submit(worker);

		verify(statisticsSender, timeout(TimeUnit.SECONDS.toMillis(20))).sendStatistics(any());

		worker.stop();
	}


	@Test
	public void shouldSendStatisticsMultipleTimesWhenSendingFails()
	{
		when(backoffStrategy.shouldRetry()).thenReturn(true);
		when(backoffStrategy.errorOccurred()).thenReturn(Duration.ofMillis(1));

		when(statisticsSender.sendStatistics(any())).thenReturn(false);

		final StatisticsPublisher.StatisticsPublisherWorker worker = getTestWorker();

		executor = Executors.newSingleThreadExecutor(RegistrableThread::new);
		final Future<?> t = executor.submit(worker);

		verify(statisticsSender, timeout(TimeUnit.SECONDS.toMillis(20)).atLeast(5)).sendStatistics(any());

		worker.stop();
	}

	@Test
	public void shouldSendStatisticsOnlyWhenBackOffStrategyAllows()
	{

		when(backoffStrategy.shouldRetry()).thenReturn(true);
		when(backoffStrategy.errorOccurred()).thenReturn(Duration.ofMillis(1));
		when(statisticsSender.sendStatistics(any())).thenReturn(false);

		final StatisticsPublisher.StatisticsPublisherWorker worker = getTestWorker();

		executor = Executors.newSingleThreadExecutor(RegistrableThread::new);
		final Future<?> t = executor.submit(worker);

		verify(statisticsSender, timeout(TimeUnit.SECONDS.toMillis(20)).atLeast(2)).sendStatistics(any());

		worker.stop();
	}

	@Test
	public void shouldIncreaseTheBackOffWhenSendingIsNotSuccessful()
	{
		when(backoffStrategy.shouldRetry()).thenReturn(true, true, true, false);
		when(backoffStrategy.errorOccurred()).thenReturn(Duration.ofMillis(1));
		when(statisticsSender.sendStatistics(any())).thenReturn(false);

		final StatisticsPublisher.StatisticsPublisherWorker worker = getTestWorker();
		executor = Executors.newSingleThreadExecutor(RegistrableThread::new);
		final Future<?> t = executor.submit(worker);
		verify(backoffStrategy, timeout(TimeUnit.SECONDS.toMillis(20)).times(2)).errorOccurred();
	}

	@Test
	public void shouldResetTheBackOffStrategyOnError()
	{
		when(backoffStrategy.shouldRetry()).thenReturn(true, true, true, false);
		when(backoffStrategy.errorOccurred()).thenReturn(Duration.ofMillis(1));
		when(statisticsSender.sendStatistics(any())).thenReturn(false);

		final StatisticsPublisher.StatisticsPublisherWorker worker = getTestWorker();

		executor = Executors.newSingleThreadExecutor(RegistrableThread::new);
		final Future<?> t = executor.submit(worker);

		verify(backoffStrategy, timeout(TimeUnit.SECONDS.toMillis(20)).times(1)).resetBackOffState();

	}

	@Test
	public void shouldResetTheBackOffStrategyOnSuccess()
	{
		when(backoffStrategy.shouldRetry()).thenReturn(true);
		when(statisticsSender.sendStatistics(any())).thenReturn(true);

		final StatisticsPublisher.StatisticsPublisherWorker worker = getTestWorker();


		executor = Executors.newSingleThreadExecutor(RegistrableThread::new);
		final Future<?> t = executor.submit(worker);

		verify(backoffStrategy, timeout(TimeUnit.SECONDS.toMillis(20)).times(1)).resetBackOffState();
		worker.stop();
	}


	@Test
	public void shouldTryToSendInDefinedIntervals()
	{
		when(backoffStrategy.shouldRetry()).thenReturn(true);
		when(statisticsSender.sendStatistics(any())).thenReturn(true);

		final Duration expectedInterval = Duration.ofMillis(50);
		final TestWorker worker = getTestWorker(expectedInterval);

		executor = Executors.newSingleThreadExecutor(RegistrableThread::new);
		final Future<?> t = executor.submit(worker);

		verify(statisticsSender, timeout(TimeUnit.SECONDS.toMillis(5)).atLeast(10)).sendStatistics(any());

		final List<Duration> waitingTimes = worker.getWaitingTimes();

		final Optional<Duration> fastest = waitingTimes.stream().min(Duration::compareTo);
		assertThat(fastest).isPresent();
		assertThat(fastest.get()).isGreaterThanOrEqualTo(expectedInterval);

		final OptionalDouble avgTime = waitingTimes.stream().mapToLong(Duration::toMillis).average();
		assertThat(avgTime).isPresent();
		assertThat(avgTime.getAsDouble()).isCloseTo(expectedInterval.toMillis(), withinPercentage(15));
		worker.stop();
	}

	private TestWorker getTestWorker(final Duration sendingInterval)
	{
		return new TestWorker(
				statisticsSender, backoffStrategy, sendingInterval);
	}

	private TestWorker getTestWorker()
	{
		return new TestWorker(
				statisticsSender, backoffStrategy, Duration.ofHours(1));
	}

	public static class TestWorker extends StatisticsPublisher.StatisticsPublisherWorker
	{

		List<Duration> wholeWaitTime = new ArrayList<>();

		TestWorker(
				final StatisticsPublisher.StatisticsSender statisticsSender,
				final BackoffStrategy backoffStrategy, final Duration sendingInterval)
		{
			super(statisticsSender, backoffStrategy, sendingInterval);
		}

		@Override
		public Duration getDefaultPartialTimeToWait()
		{
			return Duration.ofMillis(500);
		}

		List<Duration> getWaitingTimes()
		{
			return wholeWaitTime;
		}

		@Override
		void waitForNextStatsPublishing()
		{
			final Stopwatch started = Stopwatch.createStarted();
			super.waitForNextStatsPublishing();
			wholeWaitTime.add(started.stop().elapsed());
		}

		@Override
		StatisticsPayload getStatisticsPayload()
		{
			return null;
		}
	}
}
