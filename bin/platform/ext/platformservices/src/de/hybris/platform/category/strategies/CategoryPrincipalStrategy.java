/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.category.strategies;

import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.core.model.security.PrincipalModel;

import java.util.List;


/**
 * This strategy provides functionality around category and its principals. The principals can be set or added to the
 * category so that the user rights can be defined specifically.
 *
 * @spring.bean categoryPrincipalStrategy
 */
public interface CategoryPrincipalStrategy
{

	/**
	 * Replaces the principals of the specific {@link CategoryModel}.
	 */
	void replacePrincipalsForCategory(CategoryModel category, List<PrincipalModel> newPrincipals);

	/**
	 * Replaces the principals of the specific {@link CategoryModel} and the principals down to all sub-categories and up
	 * to the super categories as well.
	 */
	void replacePrincipalsForAllRelatedCategories(CategoryModel category, List<PrincipalModel> newPrincipals);

}
