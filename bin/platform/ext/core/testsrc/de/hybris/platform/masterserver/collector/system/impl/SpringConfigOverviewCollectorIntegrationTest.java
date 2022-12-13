/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.masterserver.collector.system.impl;

import static de.hybris.platform.masterserver.collector.system.impl.SpringConfigOverviewCollector.Configuration.SPRING_OVERVIEW_COLLECTOR_ENABLED_FLAG;
import static de.hybris.platform.masterserver.collector.system.impl.SpringConfigOverviewCollector.OVERVIEW_OBJECT_KEY;
import static de.hybris.platform.masterserver.collector.system.impl.SpringConfigOverviewCollector.OVERVIEW_STATS_ENCODING_KEY;
import static de.hybris.platform.masterserver.collector.system.impl.SpringConfigOverviewCollector.OVERVIEW_STATS_KEY;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.nio.charset.Charset;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class SpringConfigOverviewCollectorIntegrationTest extends ServicelayerBaseTest
{
	private PropertyConfigSwitcher enabledToggle;

	@Before
	public void setUp()
	{
		enabledToggle = new PropertyConfigSwitcher(SPRING_OVERVIEW_COLLECTOR_ENABLED_FLAG);
	}

	@After
	public void tearDown()
	{
		enabledToggle.switchBackToDefault();
	}

	@Test
	public void shouldReturnOverviewContainingGlobalAndApplicationBeansWhenEnabled()
	{
		enabledToggle.switchToValue(Boolean.TRUE.toString());
		final SpringConfigOverviewCollector collector = givenSpringConfigOverviewCollector();

		final Map<String, Map<String, String>> springStats = collector.collectStatistics();
		assertThat(springStats).isNotNull().isNotEmpty().containsKey(OVERVIEW_OBJECT_KEY);

		final Map<String, String> stats = springStats.get(OVERVIEW_OBJECT_KEY);
		assertThat(stats).isNotNull().isNotEmpty().containsKeys(OVERVIEW_STATS_KEY, OVERVIEW_STATS_ENCODING_KEY);

		assertThat(stats.get(OVERVIEW_STATS_KEY)).isNotNull().isNotEmpty();
		assertThat(stats.get(OVERVIEW_STATS_ENCODING_KEY)).isNotNull().isNotEmpty();

		final Charset encoding = Charset.forName(stats.get(OVERVIEW_STATS_ENCODING_KEY));
		assertThat(encoding).isNotNull();

		final byte[] compressedOverview = SpringOverviewTestHelper.base64Decode(stats.get(OVERVIEW_STATS_KEY));
		assertThat(compressedOverview).isNotNull().isNotEmpty();

		final String decompressedOverview = SpringOverviewTestHelper.decompress(compressedOverview, encoding);
		assertThat(decompressedOverview).isNotNull().isNotEmpty().contains("globalHybrisProperties", "applicationHybrisProperties");
	}

	@Test
	public void shouldReturnEmptyMapWhenDisabled()
	{
		enabledToggle.switchToValue(Boolean.FALSE.toString());
		final SpringConfigOverviewCollector collector = givenSpringConfigOverviewCollector();

		final Map<String, Map<String, String>> stats = collector.collectStatistics();
		assertThat(stats).isNotNull().isEmpty();
	}

	private SpringConfigOverviewCollector givenSpringConfigOverviewCollector()
	{
		return new SpringConfigOverviewCollector();
	}
}
