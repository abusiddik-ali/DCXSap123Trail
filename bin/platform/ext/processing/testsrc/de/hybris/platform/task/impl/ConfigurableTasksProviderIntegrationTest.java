/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import java.util.Map;

import javax.annotation.Resource;

import org.junit.Test;

@IntegrationTest
public class ConfigurableTasksProviderIntegrationTest extends ServicelayerBaseTest
{
	@Resource
	private DefaultTasksProvider defaultTasksProvider;
	@Resource
	private AuxiliaryTablesBasedTaskProvider auxiliaryTablesBasedTaskProvider;
	@Resource
	private InMemoryTasksProvider inMemoryTasksProvider;
	@Resource
	private ConfigurableTasksProvider configurableTasksProvider;

	@Test
	public void shouldContainDefinedTasksProviders()
	{
		final Map<String, TasksProvider> providers = configurableTasksProvider.getProviders();

		assertThat(providers).containsEntry("defaultTasksProvider", defaultTasksProvider);
		assertThat(providers).containsEntry("auxiliaryTablesBasedTaskProvider", auxiliaryTablesBasedTaskProvider);
		assertThat(providers).containsEntry("inMemoryTasksProvider", inMemoryTasksProvider);

	}
}
