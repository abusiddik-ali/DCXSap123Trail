/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.interceptors;

import de.hybris.platform.catalog.daos.ItemSyncTimestampDao;
import de.hybris.platform.catalog.model.ItemSyncTimestampModel;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PersistenceOperation;
import de.hybris.platform.servicelayer.interceptor.RemoveInterceptor;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.util.config.PropertyActionReader;

import java.util.List;

import org.springframework.beans.factory.annotation.Required;


/**
 * Remove {@link ItemSyncTimestampModel}s from {@link ItemModel}.
 */
public class SyncTimestampsRemoveInterceptor implements RemoveInterceptor
{
	private static final String SYNC_TIMESTAMPS_REMOVED_FOR = "sync.timestamps.removed.for.";

	private ItemSyncTimestampDao itemSyncTimestampDao;
	private SessionService sessionService;
	private ConfigurationService configurationService;
	private PropertyActionReader propertyActionReader;

	private Integer limit = Integer.valueOf(1000);

	@Override
	public void onRemove(final Object model, final InterceptorContext ctx) throws InterceptorException
	{
		if (shouldNotRemoveSyncItemsForType(model, ctx))
		{
			return;
		}

		List<ItemSyncTimestampModel> records = null;

		do
		{
			records = itemSyncTimestampDao.findSyncTimestampsByItem((ItemModel) model, limit.intValue());

			for (final ItemSyncTimestampModel itemSyncTimestamp : records)
			{
				ctx.registerElementFor(itemSyncTimestamp, PersistenceOperation.DELETE);
			}
		}
		while (records.size() == limit.intValue());

		markAsRemovedInContext(model);
	}

	protected boolean shouldNotRemoveSyncItemsForType(final Object model, final InterceptorContext ctx)
	{
		final String type = ctx.getModelService().getModelType(model);
		return type != null && propertyActionReader.isActionDisabledForType("synctimestamp.removal", type);
	}

	protected void markAsRemovedInContext(final Object model)
	{
		if (configurationService.getConfiguration().getBoolean("synctimestamp.query.removal.optimisation", true))
		{
			final PK pk = ((ItemModel) model).getPk();
			if (pk != null)
			{
				sessionService.setAttribute(SYNC_TIMESTAMPS_REMOVED_FOR + pk.toString(), true);
			}
		}
	}

	@Required
	public void setItemSyncTimestampDao(final ItemSyncTimestampDao itemSyncTimestampDao)
	{
		this.itemSyncTimestampDao = itemSyncTimestampDao;
	}

	@Required
	public void setSessionService(final SessionService sessionService)
	{
		this.sessionService = sessionService;
	}

	@Required
	public void setConfigurationService(final ConfigurationService configurationService)
	{
		this.configurationService = configurationService;
	}

	@Required
	public void setPropertyActionReader(final PropertyActionReader propertyActionReader)
	{
		this.propertyActionReader = propertyActionReader;
	}

	public void setLimit(final Integer limit)
	{
		this.limit = limit;
	}

}
