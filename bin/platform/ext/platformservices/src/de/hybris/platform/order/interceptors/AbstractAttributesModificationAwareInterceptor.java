/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.interceptors;

import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;

import java.util.Collection;

import org.apache.log4j.Logger;


/**
 *
 */
public abstract class AbstractAttributesModificationAwareInterceptor
{

	protected boolean isOneOfAttributesModified(final ItemModel order, final Collection<String> attributes,
	                                            final InterceptorContext ctx)
	{
		for (final String attribute : attributes)
		{
			if (isAttributeModified(order, attribute, ctx))
			{
				return true;
			}
		}
		return false;
	}

	protected boolean isAttributeModified(final ItemModel order, final String attribute, final InterceptorContext ctx)
	{
		final boolean result = ctx.isModified(order, attribute);
		if (result)
		{
			getLogger().debug("Attribute [" + attribute + "] was modified");
		}
		return result;
	}

	public abstract Logger getLogger();
}
