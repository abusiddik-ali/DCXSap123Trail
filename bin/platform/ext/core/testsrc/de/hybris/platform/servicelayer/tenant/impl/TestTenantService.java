/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.tenant.impl;

import de.hybris.platform.core.Tenant;
import de.hybris.platform.servicelayer.tenant.TenantService;

import org.springframework.beans.factory.annotation.Required;


/**
 *
 */
public class TestTenantService implements TenantService
{
	private Tenant tenant;

	@Required
	public void setCurrentTenant(final Tenant tenant)
	{
		this.tenant = tenant;
	}

	@Override
	public String getCurrentTenantId()
	{
		return tenant.getTenantID();
	}

}
