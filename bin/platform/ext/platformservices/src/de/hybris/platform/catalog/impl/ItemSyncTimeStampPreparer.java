/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.impl;

import de.hybris.platform.catalog.model.ItemSyncTimestampModel;
import de.hybris.platform.catalog.model.SyncItemJobModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;


/**
 * PrepareInterceptor for the {@link ItemSyncTimestampModel}. Sets the source and target catalog versions.
 */
public class ItemSyncTimeStampPreparer implements PrepareInterceptor
{
	@Override
	public void onPrepare(final Object model, final InterceptorContext ctx) throws InterceptorException
	{
		if (model instanceof ItemSyncTimestampModel && ctx.isNew(model))
		{
			final ItemSyncTimestampModel syncTS = (ItemSyncTimestampModel) model;

			final SyncItemJobModel syncJob = syncTS.getSyncJob();

			if (syncJob != null)
			{
				if (syncTS.getSourceVersion() == null)
				{
					syncTS.setSourceVersion(syncJob.getSourceVersion());
				}
				if (syncTS.getTargetVersion() == null)
				{
					syncTS.setTargetVersion(syncJob.getTargetVersion());
				}
			}
		}
	}
}
