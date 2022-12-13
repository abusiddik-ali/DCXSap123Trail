/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.product.interceptors;

import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.type.TypeService;
import de.hybris.platform.variants.model.VariantProductModel;
import de.hybris.platform.variants.model.VariantTypeModel;

import org.springframework.beans.factory.annotation.Required;


/**
 * Interceptor, which makes some additional operations on variant types, which are being created.
 */
public class VariantTypePrepareInterceptor implements PrepareInterceptor
{

	private TypeService typeService;

	@Required
	public void setTypeService(final TypeService typeService)
	{
		this.typeService = typeService;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void onPrepare(final Object model, final InterceptorContext ctx) throws InterceptorException
	{
		if (model instanceof VariantTypeModel && ctx.isNew(model))
		{
			final VariantTypeModel variantTypeModel = (VariantTypeModel) model;
			final ComposedTypeModel superType = variantTypeModel.getSuperType();
			if (superType == null)
			{
				variantTypeModel.setSuperType(typeService.getComposedTypeForCode(VariantProductModel._TYPECODE));
			}
			else
			{
				//is superType equal to or a subtype of VariantProduct?
				if (!typeService.isAssignableFrom(typeService.getComposedTypeForCode(VariantProductModel._TYPECODE), superType))
				{
					throw new InterceptorException("Incorrect supertype " + superType.getCode() + " (has to be "
							+ VariantProductModel._TYPECODE + " or any of its subtypes)");

				}

			}
		}

	}

}
