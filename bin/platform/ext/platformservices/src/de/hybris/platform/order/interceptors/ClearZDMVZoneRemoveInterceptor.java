/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.interceptors;

import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeValueModel;
import de.hybris.platform.deliveryzone.model.ZoneModel;
import de.hybris.platform.order.daos.ZoneDeliveryModeValueDao;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.RemoveInterceptor;

import java.util.List;

import org.springframework.beans.factory.annotation.Required;


/**
 * Checks if there are any {@link ZoneDeliveryModeValueModel}s using the zone to be deleted and removes them.
 */
public class ClearZDMVZoneRemoveInterceptor implements RemoveInterceptor
{

	private ZoneDeliveryModeValueDao zoneDeliveryModeValueDao;

	@Override
	public void onRemove(final Object model, final InterceptorContext ctx) throws InterceptorException
	{
		if (model instanceof ZoneModel)
		{
			final ZoneModel zone = (ZoneModel) model;
			final List<ZoneDeliveryModeValueModel> zdmValues = zoneDeliveryModeValueDao.findZoneDeliveryModeValuesByZone(zone);
			if (zdmValues != null && !zdmValues.isEmpty())
			{
				ctx.getModelService().removeAll(zdmValues);
			}
		}
	}

	@Required
	public void setZoneDeliveryModeValueDao(final ZoneDeliveryModeValueDao zoneDeliveryModeValueDao)
	{
		this.zoneDeliveryModeValueDao = zoneDeliveryModeValueDao;
	}
}
