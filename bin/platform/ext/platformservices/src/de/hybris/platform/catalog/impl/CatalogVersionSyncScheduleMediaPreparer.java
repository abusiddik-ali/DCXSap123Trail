/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.impl;

import de.hybris.platform.catalog.constants.CatalogConstants;
import de.hybris.platform.catalog.model.synchronization.CatalogVersionSyncScheduleMediaModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.media.MediaService;

import org.apache.log4j.Logger;


/**
 * PrepareInterceptor for the {@link CatalogVersionSyncScheduleMediaModel}. Sets the catalog sync media folder.
 */
public class CatalogVersionSyncScheduleMediaPreparer implements PrepareInterceptor
{
	private static final Logger LOG = Logger.getLogger(CatalogVersionSyncScheduleMediaPreparer.class.getName());

	private MediaService mediaService;

	/**
	 * @param service the MediaService to set
	 */
	public void setMediaService(final MediaService service)
	{
		this.mediaService = service;
	}

	@Override
	public void onPrepare(final Object model, final InterceptorContext ctx) throws InterceptorException
	{
		if (model instanceof CatalogVersionSyncScheduleMediaModel)
		{
			final CatalogVersionSyncScheduleMediaModel modelImpl = (CatalogVersionSyncScheduleMediaModel) model;
			if (modelImpl.getFolder() == null)
			{
				try
				{
					modelImpl.setFolder(mediaService.getFolder(CatalogConstants.CATALOGSYNC_MEDIA_FOLDER));
				}
				catch (final Exception e)
				{
					LOG.warn("could not found media folder for identifier " + CatalogConstants.CATALOGSYNC_MEDIA_FOLDER
							+ ", details : " + e.getMessage());
					if (LOG.isDebugEnabled())
					{
						LOG.debug("could not found media folder for identifier " + CatalogConstants.CATALOGSYNC_MEDIA_FOLDER
								+ ", details :" + e.getMessage(), e);
					}
				}
			}
		}
	}
}
