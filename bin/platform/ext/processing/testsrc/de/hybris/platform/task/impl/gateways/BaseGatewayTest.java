/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl.gateways;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.task.TaskService;
import de.hybris.platform.task.impl.AuxiliaryTablesTaskProviderTestHelper;

import javax.annotation.Resource;

import org.junit.Before;
import org.springframework.jdbc.core.JdbcTemplate;


public abstract class BaseGatewayTest extends ServicelayerBaseTest
{
	@Resource
	protected TaskService taskService;

	@Resource
	protected JdbcTemplate jdbcTemplate;

	protected AuxiliaryTablesTaskProviderTestHelper testHelper;

	@Before
	public void setupBaseGatewayTest()
	{
		testHelper = new AuxiliaryTablesTaskProviderTestHelper(taskService, jdbcTemplate);
	}

	protected void disableTaskEngine()
	{

		testHelper.disableTaskEngine();
	}

	protected void enableTaskEngine()
	{
		testHelper.enableTaskEngine();
	}

	protected void assertTableExists(final String tableName)
	{

		testHelper.assertTableExists(tableName);
	}

	protected void assertTableNotExists(final String tableName)
	{
		testHelper.assertTableNotExists(tableName);
	}
}
