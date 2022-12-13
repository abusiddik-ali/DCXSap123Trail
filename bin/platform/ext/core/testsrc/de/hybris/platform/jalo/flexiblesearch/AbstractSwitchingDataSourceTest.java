/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.jalo.flexiblesearch;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.core.AbstractTenant;
import de.hybris.platform.core.DataSourceProvider;
import de.hybris.platform.core.Registry;
import de.hybris.platform.jdbcwrapper.HybrisDataSource;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import org.junit.After;
import org.junit.Before;

public abstract class AbstractSwitchingDataSourceTest extends ServicelayerBaseTest
{

	protected AbstractTenant tenant;
	private boolean defaultForceMasterMode;

	@Before
	public void setUp() throws Exception
	{
		tenant = Registry.getCurrentTenant();

		defaultForceMasterMode = tenant.isForceMaster();

		if (defaultForceMasterMode)
		{
			tenant.cancelForceMasterMode();
		}
	}

	@After
	public void tearDown()
	{
		if (defaultForceMasterMode)
		{
			tenant.forceMasterDataSource();
		}
	}

	public HybrisDataSource doWithActivatedSlaveDataSource(final DataSourceProvider tenant, final String dataSourceId,
	                                                       final Runnable logic)
	{
		return doWithActivatedDataSource(tenant, dataSourceId, false, logic);
	}

	public HybrisDataSource doWithActivatedSlaveDataSource(final String dataSourceId, final Runnable logic)
	{
		return doWithActivatedDataSource(tenant, dataSourceId, false, logic);
	}

	public HybrisDataSource doWithActivatedAlternativeDataSource(final DataSourceProvider tenant, final String dataSourceId,
	                                                             final Runnable logic)
	{
		return doWithActivatedDataSource(tenant, dataSourceId, true, logic);
	}

	public HybrisDataSource doWithActivatedAlternativeDataSource(final String dataSourceId, final Runnable logic)
	{
		return doWithActivatedDataSource(tenant, dataSourceId, true, logic);
	}

	private HybrisDataSource doWithActivatedDataSource(final DataSourceProvider tenant, final String dataSourceId,
	                                                   final boolean isMaster, final Runnable logic)
	{
		final HybrisDataSource activatedDS;
		try
		{
			activatedDS = activateDataSource(tenant, dataSourceId, isMaster);
			logic.run();
		}
		finally
		{
			tenant.deactivateAlternativeDataSource();
		}
		return activatedDS;
	}


	private HybrisDataSource activateDataSource(final DataSourceProvider tenant, final String dataSourceId,
	                                            final boolean isMaster)
	{
		final HybrisDataSource defaultDS = tenant.getDataSource();

		if (isMaster)
		{
			tenant.activateAlternativeMasterDataSource(dataSourceId);
		}
		else
		{
			tenant.activateSlaveDataSource(dataSourceId);
		}
		final HybrisDataSource activatedDS = tenant.getDataSource();

		assertThat(activatedDS).isNotEqualTo(defaultDS).extracting(HybrisDataSource::getID).contains(dataSourceId);
		return activatedDS;
	}
}
