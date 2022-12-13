/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.classification.daos;

import de.hybris.platform.catalog.model.classification.ClassificationSystemModel;

import java.util.Collection;


/**
 * DAO for the {@link ClassificationSystemModel}.
 *
 * @spring.bean classificationSystemDao
 */
public interface ClassificationSystemDao
{

	/**
	 * Finds all {@link ClassificationSystemModel}s with the given id.
	 *
	 * @param id ClassificationSystem id
	 * @return matching {@link ClassificationSystemModel}s
	 */
	Collection<ClassificationSystemModel> findSystemsById(String id);

}
