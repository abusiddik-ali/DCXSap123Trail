/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;


import de.hybris.bootstrap.annotations.IntegrationTest;


@IntegrationTest
public class DefaultTaskServiceNodeGroupsDefaultTasksProviderIntegrationTest extends DefaultTaskServiceNodeGroupsIntegrationTest
{

	@Override
	protected TasksProvider getTasksProvider()
	{
		final DefaultTasksProvider tasksProvider = new DefaultTasksProvider();
		tasksProvider.setMetricRegistry(metricRegistry);
		tasksProvider.setTypeService(typeService);
		tasksProvider.setFlexibleSearchService(flexibleSearchService);

		tasksProvider.afterPropertiesSet();

		return tasksProvider;
	}

}
