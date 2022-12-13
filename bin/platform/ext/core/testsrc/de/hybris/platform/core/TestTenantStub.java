/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core;

import de.hybris.platform.jalo.ConsistencyCheckException;
import de.hybris.platform.util.config.ConfigIntf;

import java.util.List;
import java.util.Locale;
import java.util.TimeZone;


/**
 *
 */
public class TestTenantStub extends AbstractTenant
{
	private final AbstractTenant testTenant;

	public TestTenantStub(final String tenanId, final AbstractTenant testTenant)
	{
		super(tenanId);
		this.testTenant = testTenant;

	}

	@Override
	public TimeZone getTenantSpecificTimeZone()
	{
		return testTenant.getTenantSpecificTimeZone();
	}

	@Override
	public Locale getTenantSpecificLocale()
	{
		return testTenant.getTenantSpecificLocale();
	}

	@Override
	public void startUp() throws ConsistencyCheckException
	{
		// nothing
	}

	@Override
	public void shutDown()
	{
		// nothing
	}

	@Override
	public List<String> getTenantSpecificExtensionNames()
	{
		return testTenant.getTenantSpecificExtensionNames();
	}

	@Override
	public ConfigIntf getConfig()
	{
		return testTenant.getConfig();
	}

	@Override
	public int getClusterID()
	{
		return testTenant.getClusterID() + 1;
	}

	@Override
	void shutDown(final ShutDownMode mode)
	{
		// YTODO Auto-generated method stub

	}

}
