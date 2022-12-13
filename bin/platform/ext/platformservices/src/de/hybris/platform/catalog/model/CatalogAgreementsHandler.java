/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.model;


import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.Collection;


public class CatalogAgreementsHandler implements DynamicAttributeHandler<Collection<AgreementModel>, CatalogModel>
{
	@Override
	public Collection<AgreementModel> get(final CatalogModel model)
	{
		final CatalogVersionModel activeCatalogVersion = model.getActiveCatalogVersion();
		if (activeCatalogVersion != null)
		{
			return activeCatalogVersion.getAgreements();
		}
		return null;
	}

	@Override
	public void set(final CatalogModel model, final Collection<AgreementModel> currencyModel)
	{
	}

}
