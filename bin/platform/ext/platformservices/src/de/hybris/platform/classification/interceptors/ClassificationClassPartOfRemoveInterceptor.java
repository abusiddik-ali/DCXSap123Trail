/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.classification.interceptors;

import de.hybris.platform.catalog.model.classification.ClassificationClassModel;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.impl.PartOfModelRegisterForRemoveInterceptor;

public class ClassificationClassPartOfRemoveInterceptor extends PartOfModelRegisterForRemoveInterceptor
{
	@Override
	protected void registerForRemoval(final InterceptorContext ctx, final ItemModel parentModel, final ItemModel subModel,
	                                  final String partOfAttrQualifier)
	{
		if(!ClassificationClassModel.ALLCLASSIFICATIONATTRIBUTEASSIGNMENTS.equals(partOfAttrQualifier))
		{
			super.registerForRemoval(ctx, parentModel, subModel, partOfAttrQualifier);
		}
	}
}
