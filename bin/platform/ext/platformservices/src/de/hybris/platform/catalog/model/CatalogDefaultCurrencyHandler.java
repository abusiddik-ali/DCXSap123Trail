/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.model;


import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;


public class CatalogDefaultCurrencyHandler implements DynamicAttributeHandler<CurrencyModel, CatalogModel>
{
	@Override
	public CurrencyModel get(final CatalogModel model)
	{
		final CatalogVersionModel activeCatalogVersion = model.getActiveCatalogVersion();
		if (activeCatalogVersion != null)
		{
			return activeCatalogVersion.getDefaultCurrency();
		}
		return null;
	}

	@Override
	public void set(final CatalogModel model, final CurrencyModel currencyModel)
	{
	}

}
