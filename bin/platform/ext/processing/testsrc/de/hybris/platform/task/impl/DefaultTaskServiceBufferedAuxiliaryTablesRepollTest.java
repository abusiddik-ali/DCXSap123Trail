/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;

@IntegrationTest
public class DefaultTaskServiceBufferedAuxiliaryTablesRepollTest extends DefaultTaskServiceAuxiliaryTablesRepollTest
{
	private final PropertyConfigSwitcher schedulerInterval = new PropertyConfigSwitcher(
			AuxiliaryTablesBasedTaskProvider.Params.SCHEDULER_INTERVAL.getName());

	@Override
	@Before
	public void setUp()
	{
		schedulerInterval.switchToValue("2");
		super.setUp();
	}

	@Override
	@After
	public void tearDown()
	{
		super.tearDown();
		schedulerInterval.switchBackToDefault();
	}

	@Override
	protected TasksProvider getTasksProvider()
	{
		//this gives a new instance of stateful AuxiliaryTablesBasedTaskProvider
		final TasksProvider tasksProvider = super.getTasksProvider();

		assertThat(tasksProvider).isInstanceOf(AuxiliaryTablesBasedTaskProvider.class);

		return new BufferedAuxTablesTasksProvider(((AuxiliaryTablesBasedTaskProvider) tasksProvider), metricRegistry);
	}
}
