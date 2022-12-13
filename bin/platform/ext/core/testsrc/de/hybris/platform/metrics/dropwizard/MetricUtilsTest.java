/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.metrics.dropwizard;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.contains;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.threadregistry.RegistrableThread;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;

import javax.annotation.Resource;

import org.apache.commons.lang3.RandomStringUtils;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;

import com.codahale.metrics.Meter;
import com.codahale.metrics.MetricRegistry;

@IntegrationTest
public class MetricUtilsTest extends ServicelayerBaseTest
{

	@Spy
	private final MetricRegistry testMetricRegistry = new MetricRegistry();

	@Resource
	private MetricRegistry metricRegistry;

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void shouldCreateMetricCacheWithProvidedMetricRegistry()
	{
		final MetricUtils.CachedMetrics<String> cachedMetrics = MetricUtils.metricCache(testMetricRegistry);

		final String meterName = RandomStringUtils.randomAlphabetic(15);

		final Optional<Meter> meter = cachedMetrics.getMeter(meterName, Registry.getCurrentTenantNoFallback().getTenantID(),
				ctx -> "adjusted" + ctx.getMetricKey());

		assertThat(meter).isPresent();

		verify(testMetricRegistry).meter("adjusted" + meterName);
	}

	@Test
	public void shouldCreateMetricCacheWithDefaultMetricRegistryIfNotProvided()
	{
		final MetricUtils.CachedMetrics<String> cachedMetrics = MetricUtils.metricCache();

		final String meterName = RandomStringUtils.randomAlphabetic(15);

		final Optional<Meter> meter = cachedMetrics.getMeter(meterName, Registry.getCurrentTenantNoFallback().getTenantID(),
				ctx -> MetricUtils.metricName(ctx.getMetricKey())
				                  .extension("core")
				                  .module("test")
				                  .tenant(ctx.getTenantId())
				                  .build());

		assertThat(meter).isPresent();
		assertThat(metricRegistry.getMeters().keySet().stream().filter(s -> s.contains(meterName))).hasSize(1);
	}


	@Test
	public void shouldBuildMeterNameAndCreateMeterOnlyOnceForMultipleCalls()
	{
		final MetricUtils.CachedMetrics<String> cachedMetrics = MetricUtils.metricCache(testMetricRegistry);

		final String meterName = RandomStringUtils.randomAlphabetic(15);

		final AtomicInteger counter = new AtomicInteger();

		final Optional<Meter> meter = cachedMetrics.getMeter(meterName, Registry.getCurrentTenantNoFallback().getTenantID(),
				ctx -> "adjusted" + ctx.getMetricKey() + "_" + counter.incrementAndGet());

		assertThat(meter).isPresent();
		assertThat(counter.get()).isEqualTo(1);
		verify(testMetricRegistry).meter(contains(meterName));
	}


	@Test
	public void shouldCreateMetricNameWithoutTenantIfNoActiveTenant()
			throws InterruptedException, TimeoutException, ExecutionException
	{

		final Future<String> r = Executors.newSingleThreadExecutor(RegistrableThread::new)
		                                  .submit(() -> MetricUtils.metricName("test").build());


		final String result = r.get(60, TimeUnit.SECONDS);
		assertThat(result).contains("name=test").doesNotContain("tenant");
	}
}
