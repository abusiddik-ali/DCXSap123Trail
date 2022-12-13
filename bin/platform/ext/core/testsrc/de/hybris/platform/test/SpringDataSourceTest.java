/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test;

import static org.junit.Assert.assertEquals;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.Tenant;
import de.hybris.platform.testframework.Assert;
import de.hybris.platform.testframework.HybrisJUnit4Test;

import javax.sql.DataSource;

import org.junit.Test;


@IntegrationTest
public class SpringDataSourceTest extends HybrisJUnit4Test
{
	@Test
	public void testDataSourcesDifferentPerTenant()
	{
		final DataSource springDataSourceJunit = (DataSource) Registry.getApplicationContext().getBean("dataSource");
		final DataSource hybrisDataSourceJunit = Registry.getCurrentTenant().getDataSource();
		assertEquals(springDataSourceJunit, hybrisDataSourceJunit);

		final Tenant oldTenant = Registry.getCurrentTenant();
		if (!oldTenant.equals(Registry.getMasterTenant()))
		{
			try
			{
				Registry.setCurrentTenant(Registry.getMasterTenant());
				final DataSource springDataSourceMaster = (DataSource) Registry.getApplicationContext().getBean("dataSource");
				final DataSource hybrisDataSourceMaster = Registry.getCurrentTenant().getDataSource();
				assertEquals(springDataSourceMaster, hybrisDataSourceMaster);
				Assert.assertNotEquals(springDataSourceMaster, springDataSourceJunit);
			}
			finally
			{
				Registry.setCurrentTenant(oldTenant);
			}
		}
	}
}
