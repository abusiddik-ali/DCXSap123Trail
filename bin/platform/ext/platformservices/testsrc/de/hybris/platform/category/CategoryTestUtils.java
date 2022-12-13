/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.category;

import de.hybris.platform.category.jalo.Category;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.core.PK;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;


/**
 * Utility class containing helper methods for testing Categories.
 */
public class CategoryTestUtils
{
	/**
	 * Converts Collection of CategoryModel into sorted List of PK.
	 */
	public static List<PK> convertNewCollectionToPk(final Collection<CategoryModel> collection)
	{
		final List<PK> result = new ArrayList<PK>();
		for (final CategoryModel categoryModel : collection)
		{
			result.add(categoryModel.getPk());
		}
		Collections.sort(result);
		return result;
	}

	/**
	 * Converts Collection of Category items into sorted List of PK.
	 */
	public static List<PK> convertOldCollectionToPk(final Collection<Category> collection)
	{
		final List<PK> result = new ArrayList<PK>();
		for (final Category category : collection)
		{
			result.add(category.getPK());
		}
		Collections.sort(result);
		return result;
	}
}
