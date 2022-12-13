/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.masterserver.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.threadregistry.RegistrableThread;
import de.hybris.platform.masterserver.StatisticsGatewayFactory;

import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.CountDownLatch;

import org.junit.Before;
import org.junit.Test;

import com.hybris.statistics.StatisticsGateway;


@IntegrationTest
public class DefaultStatisticsGatewayFactoryConcurrencyTest
{

	private StatisticsGatewayFactory factory;

	@Before
	public void setUp() throws Exception
	{
		factory = DefaultStatisticsGatewayFactory.getInstance();
		assertThat(factory).isNotNull();
	}

	@Test
	public void shouldGetOrCreateOnlyOneStatisticsGatewayInstanceConcurrently() throws InterruptedException
	{
		// given
		final Set<StatisticsGateway> allGateways = new CopyOnWriteArraySet<>();
		final CountDownLatch latch = new CountDownLatch(100);

		// when
		for (int i = 0; i < 100; i++)
		{
			new RegistrableThread(new Runnable()
			{

				@Override
				public void run()
				{
					try
					{
						final StatisticsGateway gateway = factory.getOrCreateStatisticsGateway();
						allGateways.add(gateway);
					}
					finally
					{
						latch.countDown();
					}
				}
			}).start();
		}
		latch.await();

		// then
		assertThat(allGateways).hasSize(1);// java.util.Set disallows duplicates - only one instance expected here
	}
}
