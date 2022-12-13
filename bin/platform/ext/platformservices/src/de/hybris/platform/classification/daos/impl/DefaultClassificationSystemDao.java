/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.classification.daos.impl;

import de.hybris.platform.catalog.model.classification.ClassificationSystemModel;
import de.hybris.platform.classification.daos.ClassificationSystemDao;
import de.hybris.platform.servicelayer.internal.dao.DefaultGenericDao;

import java.util.Collection;
import java.util.Collections;


/**
 * Default implementation of the {@link ClassificationSystemDao}.
 */
public class DefaultClassificationSystemDao extends DefaultGenericDao<ClassificationSystemModel> implements
		ClassificationSystemDao
{

	public DefaultClassificationSystemDao()
	{
		super(ClassificationSystemModel._TYPECODE);
	}

	@Override
	public Collection<ClassificationSystemModel> findSystemsById(final String id)
	{
		final Collection<ClassificationSystemModel> scripts = find(Collections.singletonMap(
				ClassificationSystemModel.ID, (Object) id));

		return scripts;
	}

}
