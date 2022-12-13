/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;

import javax.annotation.Resource;

@IntegrationTest
public class DefaultTaskServiceAuxiliaryTablesRepollTest extends DefaultTaskServiceRepollTest
{
	@Resource
	AuxiliaryTablesGatewayFactory auxiliaryTablesGatewayFactory;

	@Override
	protected TasksProvider getTasksProvider()
	{
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
