/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.interceptors;

import de.hybris.platform.catalog.model.synchronization.CatalogVersionSyncScheduleMediaModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;


public class CatalogVersionSyncScheduleMediaValidator implements ValidateInterceptor<CatalogVersionSyncScheduleMediaModel>
{
	@Override
	public void onValidate(final CatalogVersionSyncScheduleMediaModel model, final InterceptorContext ctx)
			throws InterceptorException
	{
		if (model != null && model.getCronjob() == null)
		{
			throw new InterceptorException("mssing sync cronjob for creating a new "
					+ CatalogVersionSyncScheduleMediaModel._TYPECODE);
		}
	}
}
