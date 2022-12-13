/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.job.util;

import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.model.type.ComposedTypeModel;

import java.util.List;


/**
 * Abstract of the converter from a {@link List} of the {@link ComposedTypeModel}s within a {@link CatalogVersionModel}
 * to {@link StringBuffer}.
 */
public interface ImpexScriptGenerator
{

	/**
	 * Generates a text representation of the {@link List} of {@link ComposedTypeModel}s related to
	 * {@link CatalogVersionModel}.
	 */
	StringBuilder generate(final CatalogVersionModel version, final List<ComposedTypeModel> orderedComposedTypes);

}
