/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.regioncache.generation.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.LongStream;
import java.util.stream.Stream;

import org.junit.Test;

import com.codahale.metrics.MetricRegistry;

@UnitTest
public class TypeCodeGenerationalCounterServiceConcurrencyTest
{
	private static final int THREADS_COUNT = 13;
	private static final long ITERATIONS_STEP = 1234567L;

	private static final String TENANT_1 = "master";
	private static final String TENANT_2 = "junit";

	private static final String[] TYPE_CODES = { "1", "2", "3", "4", "5" };

	@Test
	public void shouldWorkCorrectlyWhenIncrementingFromMultipleThreads() throws InterruptedException
	{
		final MetricRegistry testMetricRegistry = new MetricRegistry();
		final TypeCodeGenerationalCounterService counterService = new TypeCodeGenerationalCounterService(testMetricRegistry);
		final CountDownLatch readyLatch = new CountDownLatch(THREADS_COUNT);
		final CountDownLatch startLatch = new CountDownLatch(1);

		final List<TestThread> testThreads = LongStream.rangeClosed(1, THREADS_COUNT)
		                                               .map(i -> i * ITERATIONS_STEP)
		                                               .mapToObj(i -> new TestThread(counterService, i, readyLatch, startLatch))
		                                               .collect(Collectors.toList());

		testThreads.forEach(Thread::start);

		readyLatch.await();
		startLatch.countDown();

		for (final Thread thread : testThreads)
		{
			thread.join(200_000);
			assertThat(thread.isAlive()).isFalse();
		}

		final long sumOfGenerations = Stream.of(TENANT_1, TENANT_2)
		                                    .map(counterService::getGenerations)
		                                    .map(Map::values)
		                                    .flatMap(Collection::stream)
		                                    .mapToLong(Long::longValue)
		                                    .sum();
		System.err.println(testThreads.size() + " -> " + sumOfGenerations);
		assertThat((THREADS_COUNT + 1) * THREADS_COUNT * ITERATIONS_STEP / 2).isEqualTo(sumOfGenerations);
	}

	private static final class TestThread extends Thread
	{
		private final TypeCodeGenerationalCounterService counterService;
		private final long numberOfIterations;
		private final CountDownLatch readyLatch;
		private final CountDownLatch startLatch;
		private final Random rnd = new Random();


		TestThread(final TypeCodeGenerationalCounterService counterService, final long numberOfIterations,
		           final CountDownLatch readyLatch,
		           final CountDownLatch startLatch)
		{
			super("TestThread " + numberOfIterations);
			this.counterService = counterService;
			this.numberOfIterations = numberOfIterations;
			this.readyLatch = readyLatch;
			this.startLatch = startLatch;
		}

		@Override
		public void run()
		{
			waitForTheStartSignal();

			for (int i = 0; i < numberOfIterations; i++)
			{
				final String tenant = rnd.nextBoolean() ? TENANT_1 : TENANT_2;
				final String typeCode = TYPE_CODES[rnd.nextInt(TYPE_CODES.length)];

				counterService.incrementGeneration(typeCode, tenant);
			}
		}

		private void waitForTheStartSignal()
		{
			readyLatch.countDown();
			try
			{
				startLatch.await(5, TimeUnit.SECONDS);
			}
			catch (final InterruptedException e)
			{
				Thread.currentThread().interrupt();
				throw new RuntimeException(e);
			}
		}
	}

}
