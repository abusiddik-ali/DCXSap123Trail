/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.interceptors;

import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.core.model.order.payment.PaymentModeModel;
import de.hybris.platform.order.DeliveryModeService;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.RemoveInterceptor;

import java.util.Collection;

import org.springframework.beans.factory.annotation.Required;


/**
 *
 */
public class RemoveDeliverModesOnPaymentModeRemovalInterceptor implements RemoveInterceptor
{

	private DeliveryModeService deliveryModeService;

	@Override
	public void onRemove(final Object model, final InterceptorContext ctx) throws InterceptorException
	{
		if (model instanceof PaymentModeModel)
		{
			final PaymentModeModel paymentMode = (PaymentModeModel) model;
			final Collection<DeliveryModeModel> paymentDeliveryModes = deliveryModeService.getSupportedDeliveryModes(paymentMode);
			if (paymentDeliveryModes != null && !paymentDeliveryModes.isEmpty())
			{
				ctx.getModelService().removeAll(paymentDeliveryModes);
			}
		}
	}

	@Required
	public void setDeliveryModeService(final DeliveryModeService deliveryModeService)
	{
		this.deliveryModeService = deliveryModeService;
	}

}
