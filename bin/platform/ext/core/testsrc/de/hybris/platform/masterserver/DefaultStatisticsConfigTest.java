/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.masterserver;

import static org.assertj.core.api.Java6Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class DefaultStatisticsConfigTest extends ServicelayerBaseTest
{
	private PropertyConfigSwitcher publisherEnabledFlag;

	@Before
	public void setUp()
	{
		publisherEnabledFlag = new PropertyConfigSwitcher(DefaultStatisticsConfig.TOMCAT_STATISTICS_ENABLED);
	}

	@After
	public void tearDown()
	{
		publisherEnabledFlag.switchBackToDefault();
	}

	@Test
	public void shouldRespectConfigurationOnTheCloudEnvironment()
	{
		final DefaultStatisticsConfig cloudConfig = givenConfig(true);

		publisherEnabledFlag.switchToValue(Boolean.TRUE.toString());
		assertThat(cloudConfig.isStatisticsPublisherEnabled()).isTrue();

		publisherEnabledFlag.switchToValue(Boolean.FALSE.toString());
		assertThat(cloudConfig.isStatisticsPublisherEnabled()).isFalse();
	}

	@Test
	public void shouldRespectConfigurationOnTheOnPremEnvironment()
	{
		final DefaultStatisticsConfig onPremConfig = givenConfig(false);

		publisherEnabledFlag.switchToValue(Boolean.TRUE.toString());
		assertThat(onPremConfig.isStatisticsPublisherEnabled()).isTrue();

		publisherEnabledFlag.switchToValue(Boolean.FALSE.toString());
		assertThat(onPremConfig.isStatisticsPublisherEnabled()).isFalse();
	}

	@Test
	public void shouldEnablePublisherByDefaultOnTheCloudEnvironment()
	{
		final DefaultStatisticsConfig cloudConfig = givenConfig(true);

		publisherEnabledFlag.switchToValue("");
		assertThat(cloudConfig.isStatisticsPublisherEnabled()).isTrue();

		publisherEnabledFlag.switchToValue(null);
		assertThat(cloudConfig.isStatisticsPublisherEnabled()).isTrue();
	}

	@Test
	public void shouldDisablePublisherByDefaultOnTheOnPremEnvironment()
	{
		final DefaultStatisticsConfig onPremConfig = givenConfig(false);

		publisherEnabledFlag.switchToValue("");
		assertThat(onPremConfig.isStatisticsPublisherEnabled()).isFalse();

		publisherEnabledFlag.switchToValue(null);
		assertThat(onPremConfig.isStatisticsPublisherEnabled()).isFalse();
	}

	private DefaultStatisticsConfig givenConfig(final boolean isCloud)
	{
		return new DefaultStatisticsConfig(() -> isCloud);
	}
}