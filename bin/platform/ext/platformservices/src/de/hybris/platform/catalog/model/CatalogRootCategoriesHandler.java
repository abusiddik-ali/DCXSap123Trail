/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.model;


import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.Collections;
import java.util.List;


public class CatalogRootCategoriesHandler implements DynamicAttributeHandler<List<CategoryModel>, CatalogModel>
{
	@Override
	public List<CategoryModel> get(final CatalogModel model)
	{
		if (model.getActiveCatalogVersion() != null)
		{
			return model.getActiveCatalogVersion().getRootCategories();
		}
		return Collections.emptyList();
	}

	@Override
	public void set(final CatalogModel model, final List<CategoryModel> categoryModels)
	{
		// read-only
	}
}
