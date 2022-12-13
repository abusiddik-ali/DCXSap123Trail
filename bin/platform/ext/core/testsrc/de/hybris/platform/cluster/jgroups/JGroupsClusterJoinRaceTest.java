/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.cluster.jgroups;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.ManualTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.jdbcwrapper.HybrisDataSource;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.util.Config;

import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicInteger;

import org.jgroups.JChannel;
import org.jgroups.Message;
import org.jgroups.ReceiverAdapter;
import org.jgroups.View;
import org.junit.Before;
import org.junit.Test;


@ManualTest
public class JGroupsClusterJoinRaceTest extends ServicelayerBaseTest
{
	private static final String CHANNEL_NAME = "hybris-broadcast";

	private final ExecutorService service = Executors.newFixedThreadPool(2);

	private AtomicInteger receivedMessageCounter;
	private CyclicBarrier barrier;

	@Before
	public void initTest()
	{
		receivedMessageCounter = new AtomicInteger(0);
		barrier = new CyclicBarrier(2);
		configureJGroups();
	}

	private void configureJGroups()
	{
		final HybrisDataSource dataSource = Registry.getCurrentTenant().getDataSource();

		final Map<String, String> connParams = dataSource.getConnectionParameters();
		System.setProperty("hybris.database.driver", connParams.get(Config.SystemSpecificParams.DB_DRIVER));
		System.setProperty("hybris.database.user", connParams.get(Config.SystemSpecificParams.DB_USERNAME));
		System.setProperty("hybris.database.password", connParams.get(Config.SystemSpecificParams.DB_PASSWORD));
		System.setProperty("hybris.database.url", connParams.get(Config.SystemSpecificParams.DB_URL));
	}

	@Test
	public void shouldFormClusterWhenNodesStartedInInterval0() throws Exception
	{
		startNodesConcurrently(0);
	}

	@Test
	public void shouldFormClusterWhenNodesStartedInInterval100() throws Exception
	{
		startNodesConcurrently(100);
	}

	@Test
	public void shouldFormClusterWhenNodesStartedInInterval500() throws Exception
	{
		startNodesConcurrently(500);
	}

	@Test
	public void shouldFormClusterWhenNodesStartedInInterval1000() throws Exception
	{
		startNodesConcurrently(1000);
	}

	@Test
	public void shouldFormClusterWhenNodesStartedInInterval1500() throws Exception
	{
		startNodesConcurrently(1500);
	}

	@Test
	public void shouldFormClusterWhenNodesStartedInInterval2000() throws Exception
	{
		startNodesConcurrently(2000);
	}

	@Test
	public void shouldFormClusterWhenNodesStartedInInterval5000() throws Exception
	{
		startNodesConcurrently(5000);
	}

	public void startNodesConcurrently(final int delayMilis) throws Exception
	{
		try
		{
			final Future<JChannel> channel0Future = service.submit(
					openJChannelWithDelay("jgroups/test/hybrisnode-0.xml", "0", 0));
			final Future<JChannel> channel1Future = service
					.submit(openJChannelWithDelay("jgroups/test/hybrisnode-1.xml", "1", delayMilis));

			try (final JChannel channel0 = channel0Future.get(); final JChannel channel1 = channel1Future.get())
			{
				Thread.sleep(1000);
				channel0.send(new Message());
				channel1.send(new Message());

				Thread.sleep(5000);
				channel0.send(new Message());
				channel1.send(new Message());

				Thread.sleep(1000);

				// 6 if second node didn't join the cluster for first message exchange
				assertThat(receivedMessageCounter.get() == 6 || receivedMessageCounter.get() == 8).isTrue();
			}
		}
		finally
		{
			service.shutdown();
		}
	}

	private DelayedJChannelOpener openJChannelWithDelay(final String configuration, final String id, final int delayMilis)
	{
		return new DelayedJChannelOpener(configuration, id, receivedMessageCounter, barrier, delayMilis);
	}

	static class DelayedJChannelOpener implements Callable<JChannel>
	{
		private final String configuration;
		private final String id;
		private final AtomicInteger messagedReceived;
		private final CyclicBarrier barrier;
		private final int sleep;


		DelayedJChannelOpener(final String configuration, final String id, final AtomicInteger messagedReceived,
		                      final CyclicBarrier barrier, final int sleep)
		{
			this.configuration = configuration;
			this.id = id;
			this.messagedReceived = messagedReceived;
			this.barrier = barrier;
			this.sleep = sleep;
		}


		@Override
		public JChannel call() throws Exception
		{
			final JChannel jChannel;
			try
			{
				jChannel = new JChannel(configuration);
				jChannel.setName("hybrisnode-" + id);
				jChannel.setReceiver(new ReceiverAdapter()
				{
					public void viewAccepted(final View view)
					{
						super.viewAccepted(view);
					}

					public void receive(final Message msg)
					{
						super.receive(msg);
						messagedReceived.incrementAndGet();
					}
				});


				barrier.await();
				sleep();
				jChannel.connect(CHANNEL_NAME);
				return jChannel;
			}
			catch (final Exception e)
			{
				throw new RuntimeException("Failed to open channel");
			}
		}

		private void sleep()
		{
			try
			{
				Thread.sleep(sleep);
			}
			catch (final InterruptedException e)
			{
				throw new RuntimeException(e);
			}
		}
	}
}
