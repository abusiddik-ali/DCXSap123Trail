/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.category.attribute;

import de.hybris.platform.category.CategoryService;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.Collection;

import org.springframework.beans.factory.annotation.Required;


public class CategoryAllSupercategories implements DynamicAttributeHandler<Collection<CategoryModel>, CategoryModel>
{
	private CategoryService categoryService;

	@Override
	public Collection<CategoryModel> get(final CategoryModel category)
	{
		return categoryService.getAllSupercategoriesForCategory(category);
	}

	@Override
	public void set(final CategoryModel model, final Collection<CategoryModel> value)
	{
		throw new UnsupportedOperationException();
	}

	@Required
	public void setCategoryService(final CategoryService categoryService)
	{
		this.categoryService = categoryService;
	}
}
