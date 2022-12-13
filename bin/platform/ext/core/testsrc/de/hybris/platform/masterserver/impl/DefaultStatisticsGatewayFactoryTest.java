/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.masterserver.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.masterserver.StatisticsGatewayFactory;

import org.junit.Before;
import org.junit.Test;

import com.hybris.statistics.StatisticsGateway;


@IntegrationTest
public class DefaultStatisticsGatewayFactoryTest
{

	private StatisticsGatewayFactory factory;

	@Before
	public void setUp() throws Exception
	{
		factory = DefaultStatisticsGatewayFactory.getInstance();
		assertThat(factory).isNotNull();
	}

	@Test
	public void shouldGetOrCreateStatisticsGatewayObject()
	{
		// given
		final StatisticsGateway statisticsGateway1 = factory.getOrCreateStatisticsGateway();
		final StatisticsGateway statisticsGateway2 = factory.getOrCreateStatisticsGateway();

		// then
		assertThat(statisticsGateway1).isNotNull();
		assertThat(statisticsGateway2).isNotNull();
		assertThat(statisticsGateway1).isSameAs(statisticsGateway2);
	}

}
