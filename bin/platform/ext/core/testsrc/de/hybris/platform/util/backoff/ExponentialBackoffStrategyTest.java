/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.util.backoff;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;

import java.time.Duration;

import org.junit.Before;
import org.junit.Test;

@UnitTest
public class ExponentialBackoffStrategyTest
{
	private static final Duration DEFAULT_TIME_TO_WAIT = Duration.ofMillis(500);
	private static final int NUMBER_OF_RETRIES = 2;
	private static final double MULTIPLIER = 1.1;
	private ExponentialBackoffStrategy backoffStrategy;

	@Before
	public void setUp()
	{
		backoffStrategy = new ExponentialBackoffStrategy(NUMBER_OF_RETRIES, DEFAULT_TIME_TO_WAIT, MULTIPLIER);
	}

	@Test
	public void testMaximumUnsuccessfulCalls()
	{
		backoffStrategy.resetBackOffState();
		backoffStrategy.errorOccurred(); // 1st fail -> should retry
		assertThat(backoffStrategy.shouldRetry()).isEqualTo(true);
		backoffStrategy.errorOccurred(); // 2nd fail -> should not retry

		assertThat(backoffStrategy.shouldRetry()).isEqualTo(false);
		assertThat(multiplyDuration(DEFAULT_TIME_TO_WAIT, MULTIPLIER * MULTIPLIER)).isEqualTo(backoffStrategy.getTimeToWait());
		assertThat(backoffStrategy.getNumberOfTriesLeft()).isEqualTo(0);
	}

	@Test
	public void testOneUnsuccessfulCallThenSuccessfulCallShouldRestartBackoffStrategy()
	{
		backoffStrategy.errorOccurred();
		backoffStrategy.resetBackOffState();

		assertThat(backoffStrategy.getTimeToWait()).isEqualTo(DEFAULT_TIME_TO_WAIT);
		assertThat(backoffStrategy.getNumberOfTriesLeft()).isEqualTo(NUMBER_OF_RETRIES);
	}


	private Duration multiplyDuration(final Duration duration, final double multiplier)
	{
		final long durationInMillis = duration.toMillis();
		final double adjustedDuration = durationInMillis * multiplier;
		return Duration.ofMillis((long) adjustedDuration);
	}

}
