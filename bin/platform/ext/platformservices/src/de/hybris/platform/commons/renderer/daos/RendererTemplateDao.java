/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.commons.renderer.daos;

import de.hybris.platform.commons.model.renderer.RendererTemplateModel;

import java.util.List;


/**
 * Dao for commons renderer
 */
public interface RendererTemplateDao
{
	/**
	 * Search for RendererTemplateModels by its code.
	 *
	 * @param code template code
	 * @return result of search
	 * @throws IllegalArgumentException if code is null
	 */
	List<RendererTemplateModel> findRendererTemplatesByCode(String code);
}
