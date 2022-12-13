/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.commons.renderer.daos.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.commons.model.renderer.RendererTemplateModel;
import de.hybris.platform.commons.renderer.daos.RendererTemplateDao;
import de.hybris.platform.servicelayer.internal.dao.DefaultGenericDao;

import java.util.Collections;
import java.util.List;


/**
 * Default implementation of {@link RendererTemplateDao}
 */
public class DefaultRendererTemplateDao extends DefaultGenericDao<RendererTemplateModel> implements RendererTemplateDao
{

	public DefaultRendererTemplateDao()
	{
		this(RendererTemplateModel._TYPECODE);
	}

	public DefaultRendererTemplateDao(final String typecode)
	{
		super(typecode);
	}

	@Override
	public List<RendererTemplateModel> findRendererTemplatesByCode(final String code)
	{
		validateParameterNotNull(code, "Code must not be null!");

		return find(Collections.singletonMap(RendererTemplateModel.CODE, (Object) code));
	}
}
