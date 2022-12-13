/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.bootstrap.config.ConfigUtil;
import de.hybris.bootstrap.config.PlatformConfig;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.junit.Test;

import com.google.common.base.Joiner;
import com.google.common.collect.Lists;


/**
 *
 */
@UnitTest
public class CoreUtilitiesTest
{

	private static final String EXISTING_MANAGER_CLASS_NAME = de.hybris.platform.servicelayer.internal.jalo.ServicelayerManager.class
			.getName();

	private static final Logger LOG = Logger.getLogger(CoreUtilitiesTest.class.getName());

	private CoreUtilities utils = null;

	private static final String PROPERTY_TO_UNESCAPE = "property.to.unescape";

	private final String[] propertyValuesToUnescape =
			{
					"ENCRYPTED{AES_ETM_ECDH_P256_V1|>lDeeX\\#aT|0/Hv)/!m=g!@pSy+Rr.2dxiXNRMYFY=1x0Q=cbQ-tl|89aca+qAWTBY8&amp;P2Ec+(|hHm+ZlydhvG0%iEQT@eA}",
					"ENCRYPTED{AES_ETM_ECDH_P256_V1|>lDeeX\\\\#aT|0/Hv)/!m=g!@pSy+Rr.2dxiXNRMYFY=1x0Q=cbQ-tl|89aca+qAWTBY8&amp;P2Ec+(|hHm+ZlydhvG0%iEQT@eA}",
					"ENCRYPTED{AES_ETM_ECDH_P256_V1|>lDeeX#aT|0/Hv)/!m=g!@pSy+Rr.2dxiXNRMYFY=1x0Q=cbQ-tl|89aca+qAWTBY8&amp;P2Ec+(|hHm+ZlydhvG0%iEQT@eA}",
					"ENCRYPTED{AES_ETM_ECDH_P256_V1|>lDeeX#",
					"ENCRYPTED{AES_ETM_ECDH_P256_V1|>lDeeX",
					"ENCRYPTED{AES_ETM_ECDH_P256_V1|>lDeeX\\#aT|0/Hv)/!m=g!@pSy+Rr.2dxiXNRMYFY=1x0Q=cbQ-tl|89aca+q\\#AWTBY8&amp;P2Ec+(|hHm+ZlydhvG0%iEQT@eA}",
					"ENCRYPTED{AES_ETM_ECDH_P256_V1|>lDeeX\\\\#aT|0/Hv)/!m=g!@pSy+Rr.2dxiXNRMYFY=1x0Q=cbQ-tl|89aca+q\\\\#AWTBY8&amp;P2Ec+(|hHm+ZlydhvG0%iEQT@eA}",
					"ENCRYPTED{AES_ETM_ECDH_P256_V1|>lDeeX#aT|0/Hv)/!m=g!@pSy+Rr.2dxiXNRMYFY=1x0Q=cbQ-tl|89aca+q#AWTBY8&amp;P2Ec+(|hHm+ZlydhvG0%iEQT@eA}",
					"ENCRYPTED{AES_ETM_ECDH_P256_V1|>lDeeX{tab}aT|0/Hv)/!m=g!@pSy+Rr.2dxiXNRMYFY=1x0Q=cbQ-tl|89aca+q#AWTBY8&amp;P2Ec+(|hHm+ZlydhvG0%iEQT@eA}"
			};

	@Test
	public void testGetAllConfiguredExtensionNames()
	{
		utils = new CoreUtilities(ConfigUtil.getPlatformConfig(CoreUtilitiesTest.class), true, 7)
		{

			@Override
			boolean isCorePropertiesNotLoaded()
			{
				return true;
			}

			@Override
			Map<String, Class> getInstalledExtensionClassMapping()
			{
				return getInstalledExtensionClassMappingNoCache();
			}

			@Override
			public List<String> getAllConfiguredExtensionNames() throws IllegalStateException
			{
				return Lists.newArrayList(getInstalledExtensionClassMappingNoCache().keySet());
			}

			@Override
			Properties loadPlatformPropertiesOnce(final PlatformConfig config)
			{

				final Properties props = new Properties();
				props.putAll(Collections.singletonMap("extension.envs", "foo," + EXISTING_MANAGER_CLASS_NAME + ";" + //
						"bar," + EXISTING_MANAGER_CLASS_NAME + ";" + //
						"baz," + EXISTING_MANAGER_CLASS_NAME + ";" + //
						"fyie," + EXISTING_MANAGER_CLASS_NAME + ";"//

				));
				return props;
			}
		};

		LOG.info(Joiner.on(";").join("Extensions :", utils.getAllConfiguredExtensionNames()));


		assertThat(utils.getAllConfiguredExtensionNames()).containsOnly("foo", "bar", "baz", "fyie");

	}

	@Test
	public void shouldBeTheSamePropertyValueAfterUnescapingHashSignForCommerceAndXmlFile()
	{
		for (final String propertyValue : propertyValuesToUnescape)
		{
			//when
			final String propertyForCommerce = getPropertyValueForCommerceAfterUnescaping(propertyValue);
			final String propertyForXml = ConfigUtil.unescapeProperty(propertyValue);

			//then
			assertThat(propertyForCommerce).isEqualTo(propertyForXml);
		}
	}

	private String getPropertyValueForCommerceAfterUnescaping(final String propertyValue)
	{
		utils = new CoreUtilities(ConfigUtil.getPlatformConfig(CoreUtilitiesTest.class), true, 7)
		{
			@Override
			Properties loadRuntimeProperties(final PlatformConfig config)
			{
				final Properties props = new Properties();
				props.setProperty(PROPERTY_TO_UNESCAPE, propertyValue);
				return props;
			}

			@Override
			boolean isCorePropertiesNotLoaded()
			{
				return true;
			}
		};

		final Properties properties = utils.loadPlatformPropertiesOnce(ConfigUtil.getPlatformConfig(CoreUtilitiesTest.class));
		return properties.getProperty(PROPERTY_TO_UNESCAPE);
	}
}
