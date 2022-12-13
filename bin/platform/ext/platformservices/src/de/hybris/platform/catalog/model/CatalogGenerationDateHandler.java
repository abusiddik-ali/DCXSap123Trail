/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.model;


import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.Date;


public class CatalogGenerationDateHandler implements DynamicAttributeHandler<Date, CatalogModel>
{
	@Override
	public Date get(final CatalogModel model)
	{
		final CatalogVersionModel activeCatalogVersion = model.getActiveCatalogVersion();
		if (activeCatalogVersion != null)
		{
			return activeCatalogVersion.getGenerationDate();
		}
		return null;
	}

	@Override
	public void set(final CatalogModel model, final Date date)
	{
	}
}
