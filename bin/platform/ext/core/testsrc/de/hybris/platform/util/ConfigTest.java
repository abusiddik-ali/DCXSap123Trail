/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util;

import static org.assertj.core.api.Java6Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class ConfigTest extends ServicelayerBaseTest
{
	private PropertyConfigSwitcher flag1;
	private PropertyConfigSwitcher flag2;

	@Before
	public void setUp()
	{
		flag1 = new PropertyConfigSwitcher(Config.MODELT_DETECTION_FLAG1);
		flag2 = new PropertyConfigSwitcher(Config.MODELT_DETECTION_FLAG2);
	}

	@After
	public void tearDown()
	{
		flag1.switchBackToDefault();
		flag2.switchBackToDefault();
	}

	@Test
	public void shouldDetectOnPremEnvironmentWhenBothModelTPropertiesAreNotSet()
	{
		flag1.switchToValue("");
		flag2.switchToValue("");
		assertThat(Config.isCloudEnvironment()).isFalse();

		flag1.switchToValue("");
		flag2.switchToValue(null);
		assertThat(Config.isCloudEnvironment()).isFalse();

		flag1.switchToValue(null);
		flag2.switchToValue("");
		assertThat(Config.isCloudEnvironment()).isFalse();

		flag1.switchToValue(null);
		flag2.switchToValue(null);
		assertThat(Config.isCloudEnvironment()).isFalse();
	}

	@Test
	public void shouldDetectCloudEnvironmentWhenBothModelTPropertiesAreSet()
	{
		flag1.switchToValue("f1");
		flag2.switchToValue("f2");

		assertThat(Config.isCloudEnvironment()).isTrue();
	}

	@Test
	public void shouldDetectCloudEnvironmentWhenOnlyOneModelTPropertyIsSet()
	{
		flag1.switchToValue("");
		flag2.switchToValue("f2");
		assertThat(Config.isCloudEnvironment()).isTrue();

		flag1.switchToValue(null);
		flag2.switchToValue("f2");
		assertThat(Config.isCloudEnvironment()).isTrue();

		flag1.switchToValue("f1");
		flag2.switchToValue("");
		assertThat(Config.isCloudEnvironment()).isTrue();

		flag1.switchToValue("f1");
		flag2.switchToValue(null);
		assertThat(Config.isCloudEnvironment()).isTrue();
	}

}