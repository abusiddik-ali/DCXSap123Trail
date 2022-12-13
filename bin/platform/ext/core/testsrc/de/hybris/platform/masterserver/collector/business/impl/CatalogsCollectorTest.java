/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.masterserver.collector.business.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

public class CatalogsCollectorTest extends ServicelayerTransactionalBaseTest
{
	private CatalogsCollector collector;

	@Before
	public void setUp() throws Exception
	{
		collector = new CatalogsCollector();
	}

	@Test
	public void testCollectStatistics() throws Exception
	{
		// when
		final Map<String, Map<String, Object>> result = collector.collectStatistics();

		// then
		assertThat(result).isNotNull().isNotEmpty();
		assertThat(result.get("catalog")).isNotNull().isNotEmpty();
		assertThat(result.get("catalog").get("catalogSizes")).isInstanceOf(HashMap.class);
		assertThat((Map) result.get("catalog").get("catalogSizes")).hasSize(0);
	}
}
