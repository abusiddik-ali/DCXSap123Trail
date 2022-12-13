/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.model;


import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;


public class CatalogIncludeFreightHandler implements DynamicAttributeHandler<Boolean, CatalogModel>
{
	@Override
	public Boolean get(final CatalogModel model)
	{
		final CatalogVersionModel activeCatalogVersion = model.getActiveCatalogVersion();
		if (activeCatalogVersion != null)
		{
			return activeCatalogVersion.getInclPacking();
		}
		return null;
	}

	@Override
	public void set(final CatalogModel model, final Boolean aBoolean)
	{
	}
}
