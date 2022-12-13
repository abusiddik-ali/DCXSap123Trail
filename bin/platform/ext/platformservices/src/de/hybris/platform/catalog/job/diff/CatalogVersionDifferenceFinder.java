/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.job.diff;

import de.hybris.platform.catalog.model.CatalogVersionDifferenceModel;
import de.hybris.platform.catalog.model.CompareCatalogVersionsCronJobModel;


/**
 * Abstract difference finder for {@link CompareCatalogVersionsCronJobModel} between two <code>TYPE</code>s.
 */
public interface CatalogVersionDifferenceFinder<TYPE, DIFF extends CatalogVersionDifferenceModel>
{
	/**
	 * Processes difference for a given {@link CompareCatalogVersionsCronJobModel} model.
	 */
	int processDifferences(final CompareCatalogVersionsCronJobModel model);

}
