/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.metrics.dropwizard;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyLong;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.testframework.BulkPropertyConfigSwitcher;
import de.hybris.platform.util.Config;

import java.time.Duration;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang3.RandomStringUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.codahale.metrics.MetricFilter;
import com.codahale.metrics.MetricRegistry;
import com.codahale.metrics.ScheduledReporter;

@IntegrationTest
public class MetricsReporterServiceTest extends ServicelayerBaseTest
{
	private final String testMetricsReporterName = RandomStringUtils.randomAlphabetic(15);
	private final Set<MetricsReporterService> metricsReporterServicesToDestroy = new HashSet<>();
	BulkPropertyConfigSwitcher propertyConfigSwitcher = new BulkPropertyConfigSwitcher();
	@Mock
	private MetricsReporterFactory metricsReporterFactory;
	@Mock
	private ScheduledReporter metricsReporter;
	@Mock
	private MetricRegistry metricRegistry;
	@Mock
	private MetricFilter metricFilter;

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);

		when(metricsReporterFactory.createMetricReporter(any(), any(), any(), any())).thenReturn(metricsReporter);
		when(metricsReporterFactory.getMetricFilter(any())).thenReturn(metricFilter);
	}

	@After
	public void tearDown() throws Exception
	{
		metricsReporterServicesToDestroy.forEach(MetricsReporterService::destroy);
		propertyConfigSwitcher.switchAllBack();
	}

	@Test
	public void shouldCreateConfiguredMetricReporterBasedOnDefaultConfiguration()
	{
		final String defaultMetricsReporterName = "logsMetricsReporter";
		final Duration period = Duration.ofSeconds(
				Config.getLong(String.format(MetricsReporterService.METRICS_REPORTER_PERIOD_SECONDS, defaultMetricsReporterName),
						0));

		final MetricsReporterService metricReporterService = createMetricReporterService(defaultMetricsReporterName, false);

		assertThat(metricReporterService.isReporterEnabled()).isFalse();
		assertThat(metricReporterService.getReportingPeriod()).isEqualByComparingTo(period);
		assertThat(metricReporterService.getReporterFilters()).hasSize(2).doesNotContainValue("");


		verify(metricsReporterFactory, never()).createMetricReporter(any(), any(), any(), any());
		verify(metricsReporter, never()).start(anyLong(), any());
	}

	@Test
	public void shouldUseDefaultsWhenNoConfigurationProvided()
	{
		final MetricsReporterService metricReporterService = createMetricReporterService(testMetricsReporterName);

		assertThat(metricReporterService.isReporterEnabled()).isFalse();
		assertThat(metricReporterService.getReportingPeriod()).isEqualByComparingTo(
				MetricsReporterService.DEFAULT_METRICS_REPORTER_PERIOD_SECONDS_VALUE);
		assertThat(metricReporterService.getReporterFilters()).isEmpty();

		verify(metricsReporterFactory, never()).createMetricReporter(any(), any(), any(), any());
		verify(metricsReporter, never()).start(anyLong(), any());
	}


	@Test
	public void shouldUseDefaultsWhenNoConfigurationProvidedButEnabledOnCCV2()
	{
		final Duration defaultPeriod = MetricsReporterService.DEFAULT_METRICS_REPORTER_PERIOD_SECONDS_VALUE;
		propertyConfigSwitcher.switchToValue("modelt.licence.id", "CCV2-abcdefgh");

		final MetricsReporterService metricReporterService = createMetricReporterService(testMetricsReporterName);

		assertThat(metricReporterService.isReporterEnabled()).isTrue();
		assertThat(metricReporterService.getReportingPeriod()).isEqualByComparingTo(
				defaultPeriod);
		assertThat(metricReporterService.getReporterFilters()).isEmpty();

		verify(metricsReporterFactory).createMetricReporter(eq(metricRegistry), eq(testMetricsReporterName), eq(metricFilter),
				any());
		verify(metricsReporter).start(eq(defaultPeriod.toSeconds()), eq(TimeUnit.SECONDS));
	}

	@Test
	public void shouldCreateReporterForValidConfiguration()
	{
		final Duration period = Duration.ofSeconds(10);

		setValidEnabledProperty(true);
		setValidPeriod(period);
		setFilter("someFilter", "(#metric.contains('a'))");

		final MetricsReporterService metricReporterService = createMetricReporterService(testMetricsReporterName);

		assertThat(metricReporterService.isReporterEnabled()).isTrue();
		assertThat(metricReporterService.getReportingPeriod()).isEqualByComparingTo(period);
		assertThat(metricReporterService.getReporterFilters()).hasSize(1).containsValues("(#metric.contains('a'))");

		verify(metricsReporterFactory).createMetricReporter(eq(metricRegistry), eq(testMetricsReporterName), eq(metricFilter),
				any());
		verify(metricsReporter).start(eq(period.toSeconds()), eq(TimeUnit.SECONDS));
	}

	@Test
	public void shouldNotCreateReporterForValidConfiguration()
	{
		final Duration period = Duration.ofSeconds(10);

		setValidEnabledProperty(false);
		setValidPeriod(period);
		setFilter("someFilter", "(#metric.contains('a'))");

		final MetricsReporterService metricReporterService = createMetricReporterService(testMetricsReporterName);

		assertThat(metricReporterService.isReporterEnabled()).isFalse();
		assertThat(metricReporterService.getReportingPeriod()).isEqualByComparingTo(period);
		assertThat(metricReporterService.getReporterFilters()).hasSize(1).containsValues("(#metric.contains('a'))");

		verify(metricsReporterFactory, never()).createMetricReporter(any(), any(), any(), any());
	}

	@Test
	public void shouldNotCreateReporterForInvalidEnabledConfiguration()
	{
		final Duration period = Duration.ofSeconds(10);

		setInvalidEnabledProperty();
		setValidPeriod(period);
		setFilter("someFilter", "(#metric.contains('a'))");

		final MetricsReporterService metricReporterService = createMetricReporterService(testMetricsReporterName);

		assertThat(metricReporterService.isReporterEnabled()).isFalse();

		verify(metricsReporterFactory, never()).createMetricReporter(any(), any(), any(), any());
	}

	@Test
	public void shouldUseDefaultReportingPeriodForInvalidConfigurationOnStart()
	{
		final Duration defaultPeriod = MetricsReporterService.DEFAULT_METRICS_REPORTER_PERIOD_SECONDS_VALUE;

		setValidEnabledProperty(true);
		setInvalidPeriod();
		setFilter("someFilter", "(#metric.contains('a'))");

		final MetricsReporterService metricReporterService = createMetricReporterService(testMetricsReporterName);

		assertThat(metricReporterService.isReporterEnabled()).isTrue();
		assertThat(metricReporterService.getReportingPeriod()).isEqualByComparingTo(defaultPeriod);
		assertThat(metricReporterService.getReporterFilters()).hasSize(1).containsValue("(#metric.contains('a'))");

		verify(metricsReporterFactory).createMetricReporter(eq(metricRegistry), eq(testMetricsReporterName), eq(metricFilter),
				any());
		verify(metricsReporter).start(eq(defaultPeriod.toSeconds()), eq(TimeUnit.SECONDS));
	}

	@Test
	public void shouldUseDefaultReportingPeriodForInvalidConfigurationWhileRunning()
	{
		final Duration period = Duration.ofSeconds(10);

		setValidEnabledProperty(true);
		setValidPeriod(period);
		setFilter("someFilter", "(#metric.contains('a'))");

		final MetricsReporterService metricReporterService = createMetricReporterService(testMetricsReporterName);

		assertThat(metricReporterService.getReportingPeriod()).isEqualByComparingTo(period);


		setInvalidPeriod();

		verify(metricsReporterFactory, times(2)).createMetricReporter(eq(metricRegistry), eq(testMetricsReporterName),
				eq(metricFilter), any());
		verify(metricsReporter, times(2)).start(eq(period.toSeconds()), eq(TimeUnit.SECONDS));
		verify(metricsReporter).stop();
	}

	@Test
	public void shouldRecreateReporterOnPeriodChange()
	{
		final Duration period = Duration.ofSeconds(10);
		final Duration newPeriod = Duration.ofSeconds(20);

		setValidEnabledProperty(true);
		setValidPeriod(period);
		setFilter("someFilter", "(#metric.contains('a'))");

		final MetricsReporterService metricReporterService = createMetricReporterService(testMetricsReporterName);

		assertThat(metricReporterService.getReportingPeriod()).isEqualByComparingTo(period);

		setValidPeriod(newPeriod);

		verify(metricsReporterFactory, times(2)).createMetricReporter(eq(metricRegistry), eq(testMetricsReporterName),
				eq(metricFilter), any());
		verify(metricsReporter).stop();
		verify(metricsReporter).start(eq(period.toSeconds()), eq(TimeUnit.SECONDS));
		verify(metricsReporter).start(eq(newPeriod.toSeconds()), eq(TimeUnit.SECONDS));
	}

	@Test
	public void shouldUseDefaultFilterValueWhenNoValidFilterDefined()
	{
		final Duration period = Duration.ofSeconds(10);

		setValidEnabledProperty(true);
		setValidPeriod(period);
		setFilter("someFilter1", "invalidFilterValue1");
		setFilter("someFilter2", "invalidFilterValue2");

		final MetricsReporterService metricReporterService = createMetricReporterService(testMetricsReporterName);

		final Map<String, String> reporterFilters = metricReporterService.getReporterFilters();
		assertThat(reporterFilters).hasSize(2)
		                           .containsKeys(filterName("someFilter1"), filterName("someFilter2"))
		                           .doesNotContainValue("invalidFilterValue1")
		                           .doesNotContainValue("invalidFilterValue2")
		                           .containsValues(MetricsReporterService.DEFAULT_METRICS_REPORTER_FILTER_VALUE);


		verify(metricsReporterFactory).createMetricReporter(eq(metricRegistry), eq(testMetricsReporterName), eq(metricFilter),
				any());
		verify(metricsReporterFactory).getMetricFilter(eq(reporterFilters));
		verify(metricsReporter).start(eq(period.toSeconds()), eq(TimeUnit.SECONDS));
	}

	@Test
	public void shouldUseDefaultFilterValueWhenFilterExpressionIsMalformed()
	{
		final Duration period = Duration.ofSeconds(10);

		setValidEnabledProperty(true);
		setValidPeriod(period);
		setFilter("someFilter", "(#metric.contains('a'");

		final MetricsReporterService metricReporterService = createMetricReporterService(testMetricsReporterName);

		final Map<String, String> reporterFilters = metricReporterService.getReporterFilters();
		assertThat(reporterFilters).hasSize(1)
		                           .containsKeys(filterName("someFilter"))
		                           .doesNotContainValue("(#metric.contains('a'")
		                           .containsValues(MetricsReporterService.DEFAULT_METRICS_REPORTER_FILTER_VALUE);


		verify(metricsReporterFactory).createMetricReporter(eq(metricRegistry), eq(testMetricsReporterName), eq(metricFilter),
				any());
		verify(metricsReporterFactory).getMetricFilter(eq(reporterFilters));
		verify(metricsReporter).start(eq(period.toSeconds()), eq(TimeUnit.SECONDS));
	}

	@Test
	public void shouldRecreateReporterOnFilterChange()
	{
		final Duration period = Duration.ofSeconds(10);

		setValidEnabledProperty(true);
		setValidPeriod(period);
		setFilter("someFilter1", "(#metric.contains('a'))");

		final MetricsReporterService metricReporterService = createMetricReporterService(testMetricsReporterName);

		final Map<String, String> reporterFilters = metricReporterService.getReporterFilters();
		assertThat(reporterFilters).hasSize(1).containsEntry(filterName("someFilter1"), "(#metric.contains('a'))");

		setFilter("someFilter1", "(#metric.contains('b'))");

		final Map<String, String> newReporterFilters = metricReporterService.getReporterFilters();
		assertThat(newReporterFilters).hasSize(1).containsEntry(filterName("someFilter1"), "(#metric.contains('b'))");


		verify(metricsReporterFactory, times(2)).createMetricReporter(eq(metricRegistry), eq(testMetricsReporterName),
				eq(metricFilter),
				any());
		verify(metricsReporterFactory).getMetricFilter(eq(reporterFilters));
		verify(metricsReporterFactory).getMetricFilter(eq(newReporterFilters));
		verify(metricsReporter, times(2)).start(eq(period.toSeconds()), eq(TimeUnit.SECONDS));
		verify(metricsReporter).stop();
	}

	@Test
	public void shouldRecreateReporterOnFilterAdd()
	{
		final Duration period = Duration.ofSeconds(10);

		setValidEnabledProperty(true);
		setValidPeriod(period);
		setFilter("someFilter1", "(#metric.contains('a'))");

		final MetricsReporterService metricReporterService = createMetricReporterService(testMetricsReporterName);

		final Map<String, String> reporterFilters = metricReporterService.getReporterFilters();
		assertThat(reporterFilters).hasSize(1).containsEntry(filterName("someFilter1"), "(#metric.contains('a'))");

		verify(metricsReporterFactory).getMetricFilter(eq(reporterFilters));

		setFilter("someFilter2", "(#metric.contains('b'))");

		final Map<String, String> newReporterFilters = metricReporterService.getReporterFilters();
		assertThat(newReporterFilters).hasSize(2)
		                              .containsEntry(filterName("someFilter1"), "(#metric.contains('a'))")
		                              .containsEntry(filterName("someFilter2"), "(#metric.contains('b'))");


		verify(metricsReporterFactory, times(2)).createMetricReporter(eq(metricRegistry), eq(testMetricsReporterName),
				eq(metricFilter),
				any());
		verify(metricsReporterFactory).getMetricFilter(eq(newReporterFilters));
		verify(metricsReporter, times(2)).start(eq(period.toSeconds()), eq(TimeUnit.SECONDS));
		verify(metricsReporter).stop();
	}

	@Test
	public void shouldStartReporterWhenEnablePropertySwitchedTrue()
	{
		final Duration period = Duration.ofSeconds(10);

		setValidEnabledProperty(false);
		setValidPeriod(period);
		setFilter("someFilter", "(#metric.contains('a'))");

		final MetricsReporterService metricReporterService = createMetricReporterService(testMetricsReporterName);

		assertThat(metricReporterService.getReportingPeriod()).isEqualByComparingTo(period);

		verify(metricsReporter, never()).start(anyLong(), any());
		setValidEnabledProperty(true);

		verify(metricsReporterFactory, times(1)).createMetricReporter(eq(metricRegistry), eq(testMetricsReporterName),
				eq(metricFilter), any());
		verify(metricsReporter, never()).stop();
		verify(metricsReporter).start(eq(period.toSeconds()), eq(TimeUnit.SECONDS));
	}

	@Test
	public void shouldStopReporterWhenEnablePropertySwitchedFalse()
	{
		final Duration period = Duration.ofSeconds(10);

		setValidEnabledProperty(false);
		setValidPeriod(period);
		setFilter("someFilter", "(#metric.contains('a'))");

		final MetricsReporterService metricReporterService = createMetricReporterService(testMetricsReporterName);

		assertThat(metricReporterService.getReportingPeriod()).isEqualByComparingTo(period);
		verify(metricsReporterFactory, never()).createMetricReporter(any(), any(), any(), any());
		verify(metricsReporter, never()).start(anyLong(), any());

		setValidEnabledProperty(true);

		verify(metricsReporter, never()).stop();
		verify(metricsReporter).start(eq(period.toSeconds()), eq(TimeUnit.SECONDS));
	}


	private MetricsReporterService createMetricReporterService(final String testMetricsReporterName)
	{
		return createMetricReporterService(testMetricsReporterName, true);
	}

	private MetricsReporterService createMetricReporterService(
			final String testMetricsReporterName, final boolean destroyAfterTest)
	{
		final MetricsReporterService metricsReporterService = new MetricsReporterService(metricRegistry, testMetricsReporterName,
				metricsReporterFactory);

		metricsReporterService.afterPropertiesSet();

		if (destroyAfterTest)
		{
			metricsReporterServicesToDestroy.add(metricsReporterService);
		}

		return metricsReporterService;
	}

	private void setFilter(final String filterId, final String value)
	{
		propertyConfigSwitcher.switchToValue(filterName(filterId), value);
	}

	private String filterName(final String filterId)
	{
		return String.format(MetricsReporterService.METRICS_REPORTER_FILTER, testMetricsReporterName) + filterId;
	}

	private void setValidPeriod(final Duration period)
	{
		propertyConfigSwitcher.switchToValue(
				String.format(MetricsReporterService.METRICS_REPORTER_PERIOD_SECONDS, testMetricsReporterName),
				String.valueOf(period.toSeconds()));
	}

	private void setInvalidPeriod()
	{
		propertyConfigSwitcher.switchToValue(
				String.format(MetricsReporterService.METRICS_REPORTER_PERIOD_SECONDS, testMetricsReporterName),
				"invalidPeriodValue");
	}

	private void setValidEnabledProperty(final boolean value)
	{
		propertyConfigSwitcher.switchToValue(
				String.format(MetricsReporterService.METRICS_REPORTER_ENABLED, testMetricsReporterName),
				String.valueOf(value));
	}

	private void setInvalidEnabledProperty()
	{
		propertyConfigSwitcher.switchToValue(
				String.format(MetricsReporterService.METRICS_REPORTER_ENABLED, testMetricsReporterName),
				"invalidFlagValue");
	}

}
