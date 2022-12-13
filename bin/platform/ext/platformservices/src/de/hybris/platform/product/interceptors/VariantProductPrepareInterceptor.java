/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.product.interceptors;

import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.variants.model.VariantProductModel;


public class VariantProductPrepareInterceptor implements PrepareInterceptor
{
	@Override
	public void onPrepare(final Object model, final InterceptorContext ctx) throws InterceptorException
	{
		if (!(model instanceof VariantProductModel))
		{
			return;
		}

		final VariantProductModel variantProductModel = (VariantProductModel) model;
		UpdateBaseProductModificationTime.fromPrepareInterceptor(variantProductModel, ctx).execute();
	}
}
