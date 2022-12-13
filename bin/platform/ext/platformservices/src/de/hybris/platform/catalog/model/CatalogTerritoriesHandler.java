/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.model;

import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.Collection;


public class CatalogTerritoriesHandler implements DynamicAttributeHandler<Collection<CountryModel>, CatalogModel>
{

	@Override
	public Collection<CountryModel> get(final CatalogModel model)
	{
		final CatalogVersionModel activeCatalogVersion = model.getActiveCatalogVersion();
		if (activeCatalogVersion != null)
		{
			return activeCatalogVersion.getTerritories();
		}
		return null;
	}

	@Override
	public void set(final CatalogModel model, final Collection<CountryModel> countryModels)
	{
	}
}
