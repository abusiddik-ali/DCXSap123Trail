/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.jalo.flexiblesearch.internal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.jalo.flexiblesearch.AbstractSwitchingDataSourceTest;
import de.hybris.platform.jalo.flexiblesearch.hints.Hint;
import de.hybris.platform.jdbcwrapper.HybrisDataSource;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@IntegrationTest
public class FlexibleSearchExecutorTest extends AbstractSwitchingDataSourceTest
{
	@Mock
	private ReadOnlyConditionsHelper readOnlyConditionsHelper;

	private FlexibleSearchExecutor flexibleSearchExecutor;

	@Override
	@Before
	public void setUp() throws Exception
	{
		super.setUp();

		MockitoAnnotations.initMocks(this);
		flexibleSearchExecutor = new FlexibleSearchExecutor(Registry.getCurrentTenant(), readOnlyConditionsHelper);
	}

	@Test
	public void shouldProvideActiveDataSourceWhenReadOnlyIsNotPossible()
	{
		whenReadOnlyDataSourceCannotBeUsed();

		final HybrisDataSource defaultDataSource = tenant.getDataSource();

		final HybrisDataSource dataSourceForQuery = flexibleSearchExecutor.getDataSourceForQuery(List.of());

		assertThat(dataSourceForQuery).isEqualTo(defaultDataSource);
		verify(readOnlyConditionsHelper, never()).getReadOnlyDataSource(any());
	}

	@Test
	public void shouldProvideActiveDataSourceWhenReadOnlyIsNotPossibleWithAltDSActivated()
	{
		whenReadOnlyDataSourceCannotBeUsed();

		final HybrisDataSource defaultDataSource = tenant.getDataSource();

		final AtomicReference<HybrisDataSource> returnedDataSource = new AtomicReference<>();

		final HybrisDataSource alternativeDataSource = doWithActivatedAlternativeDataSource("alt1", () -> {
			final HybrisDataSource dataSourceForQuery = flexibleSearchExecutor.getDataSourceForQuery(List.of());
			returnedDataSource.set(dataSourceForQuery);
		});

		assertThat(returnedDataSource.get()).isEqualTo(alternativeDataSource).isNotEqualTo(defaultDataSource);
		verify(readOnlyConditionsHelper, never()).getReadOnlyDataSource(any());
	}

	@Test
	public void shouldProvideActiveDataSourceWhenReadOnlyIsNotPossibleWithSlaveDSActivated()
	{
		whenReadOnlyDataSourceCannotBeUsed();

		final HybrisDataSource defaultDataSource = tenant.getDataSource();

		final AtomicReference<HybrisDataSource> returnedDataSource = new AtomicReference<>();

		final HybrisDataSource slaveDataSource = doWithActivatedSlaveDataSource("a", () -> {
			final HybrisDataSource dataSourceForQuery = flexibleSearchExecutor.getDataSourceForQuery(List.of());
			returnedDataSource.set(dataSourceForQuery);
		});

		assertThat(returnedDataSource.get()).isEqualTo(slaveDataSource).isNotEqualTo(defaultDataSource);
		verify(readOnlyConditionsHelper, never()).getReadOnlyDataSource(any());
	}

	@Test
	public void shouldProvideActiveDataSourceWhenReadOnlyIsPossibleButNotConfigured()
	{
		whenReadOnlyDataSourceCanBeUsed();
		whenReadOnlyDataSourceIsNotConfigured();

		final HybrisDataSource defaultDataSource = tenant.getDataSource();

		final HybrisDataSource dataSourceForQuery = flexibleSearchExecutor.getDataSourceForQuery(List.of());

		assertThat(dataSourceForQuery).isEqualTo(defaultDataSource);
	}

	@Test
	public void shouldProvideActiveDataSourceWhenReadOnlyIsPossibleButNotConfiguredWithAltDSActivated()
	{
		whenReadOnlyDataSourceCanBeUsed();
		whenReadOnlyDataSourceIsNotConfigured();

		final HybrisDataSource defaultDataSource = tenant.getDataSource();

		final AtomicReference<HybrisDataSource> returnedDataSource = new AtomicReference<>();

		final HybrisDataSource alternativeDataSource = doWithActivatedAlternativeDataSource("alt1", () -> {
			final HybrisDataSource dataSourceForQuery = flexibleSearchExecutor.getDataSourceForQuery(List.of());
			returnedDataSource.set(dataSourceForQuery);
		});

		assertThat(returnedDataSource.get()).isEqualTo(alternativeDataSource).isNotEqualTo(defaultDataSource);
	}

	@Test
	public void shouldProvideActiveDataSourceWhenReadOnlyIsPossibleButNotConfiguredWithSlaveDSActivated()
	{
		whenReadOnlyDataSourceCanBeUsed();
		whenReadOnlyDataSourceIsNotConfigured();

		final HybrisDataSource defaultDataSource = tenant.getDataSource();

		final AtomicReference<HybrisDataSource> returnedDataSource = new AtomicReference<>();

		final HybrisDataSource slaveDataSource = doWithActivatedSlaveDataSource("a", () -> {
			final HybrisDataSource dataSourceForQuery = flexibleSearchExecutor.getDataSourceForQuery(List.of());
			returnedDataSource.set(dataSourceForQuery);
		});

		assertThat(returnedDataSource.get()).isEqualTo(slaveDataSource).isNotEqualTo(defaultDataSource);
	}

	@Test
	public void shouldProvideReadOnlyDataSourceIfAvailable()
	{
		whenReadOnlyDataSourceCanBeUsed();
		final HybrisDataSource readOnlyDataSource = whenReadOnlyDataSourceIsConfigured();

		final HybrisDataSource defaultDataSource = tenant.getDataSource();

		final HybrisDataSource dataSourceForQuery = flexibleSearchExecutor.getDataSourceForQuery(List.of());

		assertThat(dataSourceForQuery).isEqualTo(readOnlyDataSource)
		                              .isNotEqualTo(defaultDataSource);
	}

	@Test
	public void shouldProvideReadOnlyDataSourceIfAvailableAndAltDSActivated()
	{
		whenReadOnlyDataSourceCanBeUsed();
		final HybrisDataSource readOnlyDataSource = whenReadOnlyDataSourceIsConfigured();
		final HybrisDataSource defaultDataSource = tenant.getDataSource();

		final AtomicReference<HybrisDataSource> returnedDataSource = new AtomicReference<>();

		final HybrisDataSource alternativeDataSource = doWithActivatedAlternativeDataSource("alt1", () -> {
			final HybrisDataSource dataSourceForQuery = flexibleSearchExecutor.getDataSourceForQuery(List.of());
			returnedDataSource.set(dataSourceForQuery);
		});

		assertThat(returnedDataSource.get()).isEqualTo(readOnlyDataSource)
		                                    .isNotEqualTo(defaultDataSource)
		                                    .isNotEqualTo(alternativeDataSource);
	}


	@Test
	public void shouldProvideReadOnlyDataSourceIfAvailableAndSlaveDSActivated()
	{
		whenReadOnlyDataSourceCanBeUsed();
		final HybrisDataSource readOnlyDataSource = whenReadOnlyDataSourceIsConfigured();
		final HybrisDataSource defaultDataSource = tenant.getDataSource();

		final AtomicReference<HybrisDataSource> returnedDataSource = new AtomicReference<>();

		final HybrisDataSource slaveDataSource = doWithActivatedSlaveDataSource("a", () -> {
			final HybrisDataSource dataSourceForQuery = flexibleSearchExecutor.getDataSourceForQuery(List.of());
			returnedDataSource.set(dataSourceForQuery);
		});

		assertThat(returnedDataSource.get()).isEqualTo(readOnlyDataSource)
		                                    .isNotEqualTo(defaultDataSource)
		                                    .isNotEqualTo(slaveDataSource);
	}


	private void whenReadOnlyDataSourceCannotBeUsed()
	{
		when(readOnlyConditionsHelper.couldUseReadOnlyDataSource(any(), anyListOf(Hint.class))).thenReturn(false);
	}

	private void whenReadOnlyDataSourceCanBeUsed()
	{
		when(readOnlyConditionsHelper.couldUseReadOnlyDataSource(any(), anyListOf(Hint.class))).thenReturn(true);
	}

	private void whenReadOnlyDataSourceIsNotConfigured()
	{
		when(readOnlyConditionsHelper.getReadOnlyDataSource(any())).thenReturn(Optional.empty());
	}

	private HybrisDataSource whenReadOnlyDataSourceIsConfigured()
	{
		final HybrisDataSource mockDataSource = mock(HybrisDataSource.class);
		when(readOnlyConditionsHelper.getReadOnlyDataSource(any())).thenReturn(Optional.of(mockDataSource));

		return mockDataSource;
	}
}
