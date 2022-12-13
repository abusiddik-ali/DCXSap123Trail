/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.job.callback;

import de.hybris.platform.catalog.model.RemoveCatalogVersionCronJobModel;
import de.hybris.platform.servicelayer.impex.ImportResult;


/**
 * Callback for handling 'events' during remove import cron job work.
 *
 * @spring.bean removeJobCallback
 * @since 4.3
 */
public interface RemoveCallback<P>
{
	/**
	 * Hook for the before handling.
	 */
	void beforeRemove(RemoveCatalogVersionCronJobModel cronJobModel, P parentType);

	/**
	 * Hook for the period polling current status of the import.
	 */
	void doRemove(RemoveCatalogVersionCronJobModel cronJobModel, P parentType, ImportResult result);

	/**
	 * Hook for the after import handling.
	 */
	void afterRemoved(RemoveCatalogVersionCronJobModel cronJobModel, P parentType, ImportResult result);
}
