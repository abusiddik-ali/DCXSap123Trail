/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.interceptors;

import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeValueModel;
import de.hybris.platform.order.daos.ZoneDeliveryModeValueDao;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.RemoveInterceptor;

import java.util.List;

import org.springframework.beans.factory.annotation.Required;


/**
 * Checks if there are any ZDMVM using the current Currency which is to be deleted and removes the ZDMVM before the
 * currency.
 */
public class ClearZDMVCurrencyRemoveInterceptor implements RemoveInterceptor
{

	private ZoneDeliveryModeValueDao zoneDeliveryModeValueDao;

	@Override
	public void onRemove(final Object model, final InterceptorContext ctx) throws InterceptorException
	{
		if (model instanceof CurrencyModel)
		{
			final CurrencyModel currency = (CurrencyModel) model;
			final List<ZoneDeliveryModeValueModel> zdmValues = zoneDeliveryModeValueDao
					.findZoneDeliveryModeValuesByCurrency(currency);
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
