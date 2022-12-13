/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.util.Collections;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@IntegrationTest
public class ConfigurableTasksProviderTest extends ServicelayerBaseTest
{
	@Mock
	TasksProvider provider1;
	@Mock
	TasksProvider provider2;
	@Mock
	TasksProvider defaultProvider;

	PropertyConfigSwitcher taskPollingProvider = new PropertyConfigSwitcher(
			ConfigurableTasksProvider.PROPERTY_TASK_POLLING_PROVIDER);

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);
	}

	@After
	public void tearDown() throws Exception
	{
		taskPollingProvider.switchBackToDefault();
	}

	@Test
	public void shouldThrowExceptionWhenDefaultProviderIsNull()
	{
		assertThatThrownBy(() -> new ConfigurableTasksProvider(Collections.emptyMap(), null)).isInstanceOf(
				NullPointerException.class);
	}

	@Test
	public void shouldThrowExceptionWhenProvidersMapIsNull()
	{
		assertThatThrownBy(() -> new ConfigurableTasksProvider(null, defaultProvider)).isInstanceOf(NullPointerException.class);
	}

	@Test
	public void shouldUseDefaultProviderIfPropertyIsNotSet()
	{
		taskPollingProvider.switchToValue(null);

		final ConfigurableTasksProvider configurableTasksProvider = new ConfigurableTasksProvider(
				Map.of("provider1", provider1, "provider2", provider2), defaultProvider);

		assertThat(configurableTasksProvider.getTasksProvider()).isEqualTo(defaultProvider);
	}


	@Test
	public void shouldUseDefaultProviderIfPropertyIsEmpty()
	{
		taskPollingProvider.switchToValue("");

		final ConfigurableTasksProvider configurableTasksProvider = new ConfigurableTasksProvider(
				Map.of("provider1", provider1, "provider2", provider2), defaultProvider);

		assertThat(configurableTasksProvider.getTasksProvider()).isEqualTo(defaultProvider);
	}

	@Test
	public void shouldUseDefaultProviderIfPropertyIsInvalid()
	{
		taskPollingProvider.switchToValue("notValidValue");

		final ConfigurableTasksProvider configurableTasksProvider = new ConfigurableTasksProvider(
				Map.of("provider1", provider1, "provider2", provider2), defaultProvider);

		assertThat(configurableTasksProvider.getTasksProvider()).isEqualTo(defaultProvider);
	}

	@Test
	public void shouldUseDefaultProviderIfPropertyHasInvalidCase()
	{
		taskPollingProvider.switchToValue("ProVideR1");

		final ConfigurableTasksProvider configurableTasksProvider = new ConfigurableTasksProvider(
				Map.of("provider1", provider1, "provider2", provider2), defaultProvider);

		assertThat(configurableTasksProvider.getTasksProvider()).isEqualTo(defaultProvider);
	}


	@Test
	public void shouldUseProvider1FromMapIfPropertyIsSet()
	{
		taskPollingProvider.switchToValue("provider1");

		final ConfigurableTasksProvider configurableTasksProvider = new ConfigurableTasksProvider(
				Map.of("provider1", provider1, "provider2", provider2), defaultProvider);

		assertThat(configurableTasksProvider.getTasksProvider()).isEqualTo(provider1);
	}


	@Test
	public void shouldUseProvider2FromMapIfPropertyIsSet()
	{
		taskPollingProvider.switchToValue("provider2");

		final ConfigurableTasksProvider configurableTasksProvider = new ConfigurableTasksProvider(
				Map.of("provider1", provider1, "provider2", provider2), defaultProvider);

		assertThat(configurableTasksProvider.getTasksProvider()).isEqualTo(provider2);
	}
}
