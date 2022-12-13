/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;

public class DefaultTaskServiceDefaultTaskProviderRepollTest extends DefaultTaskServiceRepollTest
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
