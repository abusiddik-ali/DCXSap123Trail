/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.model;


import de.hybris.platform.category.daos.CategoryDao;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Required;


public class CatalogVersionRootCategoriesHandler implements DynamicAttributeHandler<List<CategoryModel>, CatalogVersionModel>
{
	private static final Logger LOG = LoggerFactory.getLogger(CatalogVersionRootCategoriesHandler.class);

	private CategoryDao categoryDao;

	@Override
	public List<CategoryModel> get(final CatalogVersionModel model)
	{
		return categoryDao.findOrderedRootCategoriesForCatalogVersion(model);
	}

	@Override
	public void set(final CatalogVersionModel model, final List<CategoryModel> categoryModels)
	{
		LOG.warn("de.hybris.platform.catalog.model.CatalogVersionModel getRootCategories() method is no-op. "
				+ "RootCategories is a read-only attribute");
	}

	@Required
	public void setCategoryDao(final CategoryDao categoryDao)
	{
		this.categoryDao = categoryDao;
	}
}
