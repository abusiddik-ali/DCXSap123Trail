/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.web.session.stale.impl;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.servicelayer.web.session.stale.StaleSessionConfig;

import java.util.concurrent.TimeUnit;

import org.junit.Test;

@UnitTest
public class CachingConfigTest
{
	@Test
	public void shouldDelegateToTargetExtension()
	{
		final StaleSessionConfig targetConfig = mock(StaleSessionConfig.class);

		final CachingConfig config = new CachingConfig(targetConfig);
		config.isStaleSessionCheckingEnabled("testExtension");

		verify(targetConfig, times(1)).isStaleSessionCheckingEnabled("testExtension");
	}

	@Test
	public void shouldLimitNumberOfCallsToTargetConfig()
	{
		final StaleSessionConfig targetConfig = mock(StaleSessionConfig.class);

		final CachingConfig config = new CachingConfig(targetConfig);
		config.setTtlSeconds(2);

		for (int i = 0; i < 10; i++)
		{
			config.isStaleSessionCheckingEnabled("testExtension");
		}

		verify(targetConfig, times(1)).isStaleSessionCheckingEnabled("testExtension");
	}

	@Test
	public void shouldCallTargetConfigAfterTTL() throws InterruptedException
	{
		final StaleSessionConfig targetConfig = mock(StaleSessionConfig.class);

		final CachingConfig config = new CachingConfig(targetConfig);
		config.setTtlSeconds(2);

		config.isStaleSessionCheckingEnabled("testExtension");
		config.isStaleSessionCheckingEnabled("testExtension");
		config.isStaleSessionCheckingEnabled("testExtension");

		TimeUnit.SECONDS.sleep(4);

		config.isStaleSessionCheckingEnabled("testExtension");
		config.isStaleSessionCheckingEnabled("testExtension");
		config.isStaleSessionCheckingEnabled("testExtension");

		verify(targetConfig, times(2)).isStaleSessionCheckingEnabled("testExtension");
	}
}