/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.web.session.stale.impl;

import static de.hybris.platform.servicelayer.web.session.stale.impl.TenantAwareConfig.STALE_SESSION_GLOBAL_CONFIG;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.Config;

import org.junit.After;
import org.junit.Test;

@IntegrationTest
public class TenantAwareConfigTest extends ServicelayerBaseTest
{
	private static final String TEST_EXTENSION_NAME = "testExtension";
	private static final String EXTENSION_CONFIG_PARAM = TenantAwareConfig.getExtensionConfigParameterName(TEST_EXTENSION_NAME);
	private final PropertyConfigSwitcher globalFlag = new PropertyConfigSwitcher(STALE_SESSION_GLOBAL_CONFIG);

	@After
	public void restoreGlobalConfig()
	{
		globalFlag.switchBackToDefault();
	}

	@Test
	public void shouldReactToChangesMadeInRuntime()
	{
		final TenantAwareConfig config = givenConfig();

		Config.setParameter(EXTENSION_CONFIG_PARAM, "false");
		assertThat(config.isStaleSessionCheckingEnabled(TEST_EXTENSION_NAME)).isFalse();

		Config.setParameter(EXTENSION_CONFIG_PARAM, "true");
		assertThat(config.isStaleSessionCheckingEnabled(TEST_EXTENSION_NAME)).isTrue();
	}

	@Test
	public void shouldUseGlobalConfigurationWhenExtensionSpecificConfigurationIsMissing()
	{
		final TenantAwareConfig config = givenConfig();

		globalFlag.switchToValue("false");
		assertThat(config.isStaleSessionCheckingEnabled("ext")).isFalse();

		globalFlag.switchToValue("true");
		assertThat(config.isStaleSessionCheckingEnabled("ext")).isTrue();
	}

	@Test
	public void shouldIgnoreGlobalConfigurationWhenExtensionSpecificConfigurationIsProvided()
	{
		final TenantAwareConfig config = givenConfig();

		globalFlag.switchToValue("false");
		Config.setParameter(EXTENSION_CONFIG_PARAM, "true");
		assertThat(config.isStaleSessionCheckingEnabled(TEST_EXTENSION_NAME)).isTrue();

		globalFlag.switchToValue("true");
		Config.setParameter(EXTENSION_CONFIG_PARAM, "false");
		assertThat(config.isStaleSessionCheckingEnabled(TEST_EXTENSION_NAME)).isFalse();
	}

	private TenantAwareConfig givenConfig()
	{
		return new TenantAwareConfig();
	}

}