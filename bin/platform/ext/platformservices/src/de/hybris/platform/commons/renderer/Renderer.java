/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.commons.renderer;

import de.hybris.platform.commons.model.renderer.RendererTemplateModel;

import java.io.Writer;


/**
 * Renders template using context informations
 */
public interface Renderer
{
	/**
	 * Renders template. Rendered content is stored into output.
	 *
	 * @param template to render
	 * @param context  context (usually map of properties)
	 * @param output   rendered content
	 * @throws de.hybris.platform.commons.renderer.exceptions.RendererException if any problem during rendering occurrs
	 */
	void render(RendererTemplateModel template, Object context, Writer output);
}
