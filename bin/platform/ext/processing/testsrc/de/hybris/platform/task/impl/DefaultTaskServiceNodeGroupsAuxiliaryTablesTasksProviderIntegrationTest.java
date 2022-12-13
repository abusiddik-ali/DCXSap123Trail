/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;


import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.task.constants.TaskConstants;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import org.junit.After;
import org.junit.Before;


@IntegrationTest
public class DefaultTaskServiceNodeGroupsAuxiliaryTablesTasksProviderIntegrationTest
		extends DefaultTaskServiceNodeGroupsIntegrationTest
{

	private final PropertyConfigSwitcher taskPollInterval = new PropertyConfigSwitcher(TaskConstants.Params.POLLING_INTERVAL);
	private final PropertyConfigSwitcher schedulerInterval = new PropertyConfigSwitcher(
			AuxiliaryTablesBasedTaskProvider.Params.SCHEDULER_INTERVAL.getName());

	@Override
	@Before
	public void setUp()
	{
		super.setUp();
		taskPollInterval.switchToValue("1");
		schedulerInterval.switchToValue("2");
	}

	@Override
	@After
	public void tearDown()
	{
		super.tearDown();
		taskPollInterval.switchBackToDefault();
		schedulerInterval.switchBackToDefault();
	}

	@Override
	protected TasksProvider getTasksProvider()
	{
		final AuxiliaryTablesGatewayFactory auxiliaryTablesGatewayFactory = Registry.getApplicationContext()
		                                                                            .getBean("auxiliaryTablesGatewayFactory",
				                                                                            AuxiliaryTablesGatewayFactory.class);

		final AuxiliaryTablesSchedulerRole schedulerRole = new AuxiliaryTablesSchedulerRole();
		schedulerRole.setGatewayFactory(auxiliaryTablesGatewayFactory);
		schedulerRole.setMetricRegistry(metricRegistry);
		schedulerRole.setTypeService(typeService);
		schedulerRole.setWorkerHelper(new DefaultWorkerHelper());
		schedulerRole.afterPropertiesSet();

		final AuxiliaryTablesWorkerRole workerRole = new AuxiliaryTablesWorkerRole();
		workerRole.setGatewayFactory(auxiliaryTablesGatewayFactory);
		workerRole.setMetricRegistry(metricRegistry);
		workerRole.afterPropertiesSet();

		final AuxiliaryTablesBasedTaskProvider tasksProvider = new AuxiliaryTablesBasedTaskProvider();
		tasksProvider.setAuxiliaryTablesSchedulerRole(schedulerRole);
		tasksProvider.setAuxiliaryTablesWorkerRole(workerRole);

		return tasksProvider;
	}

}
