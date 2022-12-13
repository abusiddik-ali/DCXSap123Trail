/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.search.preprocessor;

import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.preprocessor.QueryPreprocessor;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;


/**
 * Catalog Version query preprocessor used for Flexible Search Service to add logic before executing query which puts
 * possible catalog versions in the session.
 */
public class CatalogVersionQueryPreprocessor implements QueryPreprocessor
{
	private static final Logger LOG = Logger.getLogger(CatalogVersionQueryPreprocessor.class);
	private CatalogVersionService catalogVersionService;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void process(final FlexibleSearchQuery query)
	{
		if (query.getCatalogVersions() != null && !query.getCatalogVersions().isEmpty())
		{
			if (LOG.isDebugEnabled())
			{
				LOG.debug("Storing catalog versions from query object: " + query.getCatalogVersions() + " into user session.");
			}
			catalogVersionService.setSessionCatalogVersions(query.getCatalogVersions());
		}
	}

	@Required
	public void setCatalogVersionService(final CatalogVersionService catalogVersionService)
	{
		this.catalogVersionService = catalogVersionService;
	}
}
