/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.category.daos;

import de.hybris.platform.core.model.type.ComposedTypeModel;

import java.util.List;


/**
 * Dao to manage types that are catalog aware.
 */
public interface CatalogTypeDao
{
	/**
	 * Find all composed types with CATALOGITEMTYPE = true
	 *
	 * @return the list of composed type models
	 */
	List<ComposedTypeModel> findAllCatalogVersionAwareTypes();
}
