/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.interceptors;


import de.hybris.platform.catalog.model.AgreementModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;


public class AgreementPrepareInterceptor implements PrepareInterceptor<AgreementModel>
{
	@Override
	public void onPrepare(final AgreementModel agreement, final InterceptorContext ctx) throws InterceptorException
	{
		if (agreement.getCatalogVersion() != null)
		{
			agreement.setProperty(AgreementModel.CATALOG, agreement.getCatalogVersion().getCatalog());
		}

	}
}
