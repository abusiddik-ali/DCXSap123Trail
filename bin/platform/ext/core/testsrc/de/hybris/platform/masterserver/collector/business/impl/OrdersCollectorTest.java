/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.masterserver.collector.business.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;

import java.util.Map;

import org.junit.Before;
import org.junit.Test;

public class OrdersCollectorTest extends ServicelayerTransactionalBaseTest
{
	private OrdersCollector collector;

	@Before
	public void setUp() throws Exception
	{
		collector = new OrdersCollector();
	}

	@Test
	public void testCollectStatistics() throws Exception
	{
		// when
		final Map<String, Map<String, Object>> result = collector.collectStatistics();

		// then
		assertThat(result).isNotNull().isNotEmpty();
		assertThat(result.get("orders")).isNotNull().isNotEmpty();
		assertThat(result.get("orders").get("numOrders")).overridingErrorMessage("On test systems there are no orders").isEqualTo
				(Integer.valueOf(0));
	}

}
