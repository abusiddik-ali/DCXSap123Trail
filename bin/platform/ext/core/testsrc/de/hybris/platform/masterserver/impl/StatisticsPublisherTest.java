/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.masterserver.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.masterserver.impl.StatisticsPublisher.StatisticsPublisherWorker;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.test.TestThreadsHolder;
import de.hybris.platform.testframework.BulkPropertyConfigSwitcher;
import de.hybris.platform.util.backoff.BackoffStrategy;
import de.hybris.platform.util.backoff.ExponentialBackoffStrategy;

import java.time.Duration;
import java.util.concurrent.TimeUnit;

import javax.annotation.Resource;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.RandomStringUtils;
import org.assertj.core.api.Assertions;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class StatisticsPublisherTest extends ServicelayerBaseTest
{
	private static final String TOMCAT_STATISTICS_MAX_ATTEMPTS = "tomcat.statistics.max.attempts";
	private static final String TOMCAT_STATISTICS_BACKOFF_FACTOR = "tomcat.statistics.backoff.factor";
	private static final String TOMCAT_STATISTICS_SEND_DELAY = "tomcat.statistics.send.delay";
	private static final String TOMCAT_STATISTICS_ENABLED = "tomcat.statistics.enabled";
	private final BulkPropertyConfigSwitcher properties = new BulkPropertyConfigSwitcher();
	@Resource
	private StatisticsPublisher statisticsPublisher;
	private boolean publisherWasWorking;

	@Before
	public void setUp() throws Exception
	{
		disableDefaultStatisticsPublisher();
	}

	@After
	public void tearDown() throws Exception
	{
		properties.switchAllBack();
		enableDefaultStatisticsPublisher();
	}


	@Test
	public void shouldWorkForStartingAndStoppingAndStarting()
	{
		tomcatStatisticsEnabled(true);

		final StatisticsPublisherWorker worker = mock(StatisticsPublisherWorker.class);
		final StatisticsPublisher publisher = new TestStatisticsPublisher(worker);

		publisher.startStatisticsPublisherWorker();
		verify(worker, timeout(TimeUnit.SECONDS.toMillis(20)).times(1)).run();
		publisher.stopStatisticsPublisher();
		verify(worker, timeout(TimeUnit.SECONDS.toMillis(20)).times(1)).stop();
		publisher.startStatisticsPublisherWorker();
		verify(worker, timeout(TimeUnit.SECONDS.toMillis(20)).times(2)).run();
	}

	@Test
	public void shouldNotStartWorkerWhenPropertyIsDisabled() throws InterruptedException
	{
		tomcatStatisticsEnabled(false);
		final StatisticsPublisherWorker worker = mock(StatisticsPublisherWorker.class);

		final StatisticsPublisher publisher = new TestStatisticsPublisher(worker);
		publisher.startStatisticsPublisherWorker();
		verify(worker, never()).run();
	}


	@Test
	public void shouldNotStartWorkerWhenPropertyIsInvalid()
	{
		invalidTomcatStatisticsEnabled();
		final StatisticsPublisherWorker worker = mock(StatisticsPublisherWorker.class);

		//when
		final StatisticsPublisher publisher = new TestStatisticsPublisher(worker);
		publisher.startStatisticsPublisherWorker();

		//then
		verify(worker, never()).run();
	}

	@Test
	public void shouldStartWorkerThreadIfPropertyIsTrue()
	{
		//given
		final StatisticsPublisherWorker worker = mock(StatisticsPublisherWorker.class);
		tomcatStatisticsEnabled(true);

		//when
		final StatisticsPublisher publisher = new TestStatisticsPublisher(worker);
		publisher.startStatisticsPublisherWorker();

		//then
		verify(worker, timeout(TimeUnit.SECONDS.toMillis(20))).run();
	}


	@Test
	public void shouldStopWorkerOnDestroy() throws Exception
	{
		tomcatStatisticsEnabled(true);
		final StatisticsPublisherWorker worker = mock(StatisticsPublisherWorker.class);

		final StatisticsPublisher publisher = new TestStatisticsPublisher(worker);
		publisher.startStatisticsPublisherWorker();
		verify(worker, timeout(TimeUnit.SECONDS.toMillis(20)).times(1)).run();
		publisher.destroy();
		verify(worker, timeout(TimeUnit.SECONDS.toMillis(20)).times(1)).stop();
	}

	@Test
	public void shouldStartOnlyOneWorkerWhenCalledMultipleTimes() throws InterruptedException
	{
		tomcatStatisticsEnabled(true);
		final StatisticsPublisherWorker worker = mock(StatisticsPublisherWorker.class);
		final StatisticsPublisher publisher = new TestStatisticsPublisher(worker);

		publisher.startStatisticsPublisherWorker();
		for (int i = 0; i < 5; i++)
		{
			publisher.startStatisticsPublisherWorker();
		}
		verify(worker, timeout(TimeUnit.SECONDS.toMillis(20)).times(1)).run();

	}

	@Test
	public void shouldStartOnlyOneWorkerWhenCalledMultipleTimesConcurrently()
	{
		tomcatStatisticsEnabled(true);
		final StatisticsPublisherWorker worker = mock(StatisticsPublisherWorker.class);
		final StatisticsPublisher publisher = new TestStatisticsPublisher(worker);

		final TestThreadsHolder<Runnable> threads = new TestThreadsHolder<>(5,
				publisher::startStatisticsPublisherWorker, true);

		assertThat(threads.waitForPrepared(5, TimeUnit.SECONDS)).isTrue();
		threads.startAll();

		threads.waitForAll(5, TimeUnit.SECONDS);
		verify(worker, timeout(TimeUnit.SECONDS.toMillis(20)).times(1)).run();
	}

	@Test
	public void shouldCreateBackOffStrategyWithValuesFromProperties()
	{
		setMultiplierForStrategy(1.5);
		setDefaultTimeToWaitForStrategy(45);
		setNumberOfRetiesForStrategy(100);
		final BackoffStrategy backOffStrategy = new StatisticsPublisher().createExponentialBackoffStrategy();

		Assertions.assertThat(backOffStrategy)
		          .isInstanceOf(ExponentialBackoffStrategy.class)
		          .extracting("numberOfRetries", "defaultTimeToWait", "multiplier")
		          .containsOnly(100, Duration.ofMillis(45), 1.5);

	}

	private void setNumberOfRetiesForStrategy(final int value)
	{
		properties.switchToValue(TOMCAT_STATISTICS_MAX_ATTEMPTS, String.valueOf(value));
	}

	private void setMultiplierForStrategy(final double value)
	{
		properties.switchToValue(TOMCAT_STATISTICS_BACKOFF_FACTOR, String.valueOf(value));
	}

	private void setDefaultTimeToWaitForStrategy(final long value)
	{
		properties.switchToValue(TOMCAT_STATISTICS_SEND_DELAY, String.valueOf(value));
	}

	private void tomcatStatisticsEnabled(final boolean value)
	{
		properties.switchToValue(TOMCAT_STATISTICS_ENABLED, String.valueOf(value));
	}


	private void invalidTomcatStatisticsEnabled()
	{
		properties.switchToValue(TOMCAT_STATISTICS_ENABLED, RandomStringUtils.randomAlphabetic(10));
	}

	private void enableDefaultStatisticsPublisher()
	{
		if (publisherWasWorking)
		{
			statisticsPublisher.startStatisticsPublisherWorker();
		}
	}

	private void disableDefaultStatisticsPublisher()
	{
		publisherWasWorking = statisticsPublisher.isStatisticsPublisherEnabled();
		statisticsPublisher.stopStatisticsPublisher();
	}

	public static class TestStatisticsPublisher extends StatisticsPublisher
	{
		private final StatisticsPublisherWorker worker;

		TestStatisticsPublisher(final StatisticsPublisherWorker worker)
		{
			this.worker = worker;
		}

		@Override
		protected StatisticsPublisherWorker createWorker()
		{
			return ObjectUtils.defaultIfNull(worker, super.createWorker());
		}
	}
}
