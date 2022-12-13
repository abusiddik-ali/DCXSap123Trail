/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.product.interceptors;

import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.RemoveInterceptor;
import de.hybris.platform.variants.model.VariantProductModel;


public class VariantProductRemoveInterceptor implements RemoveInterceptor
{

	@Override
	public void onRemove(final Object model, final InterceptorContext ctx) throws InterceptorException
	{
		if (!(model instanceof VariantProductModel))
		{
			return;
		}

		final VariantProductModel variantProductModel = (VariantProductModel) model;
		UpdateBaseProductModificationTime.fromRemoveInterceptor(variantProductModel, ctx).execute();
	}

}
