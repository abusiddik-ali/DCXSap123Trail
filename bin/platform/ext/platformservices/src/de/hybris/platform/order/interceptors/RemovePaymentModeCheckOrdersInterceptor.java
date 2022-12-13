/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.interceptors;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.payment.PaymentModeModel;
import de.hybris.platform.order.daos.OrderDao;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.RemoveInterceptor;

import java.util.Collection;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;


/**
 *
 */
public class RemovePaymentModeCheckOrdersInterceptor implements RemoveInterceptor
{

	private OrderDao orderDao;

	@Override
	public void onRemove(final Object model, final InterceptorContext ctx) throws InterceptorException
	{
		if (model instanceof PaymentModeModel)
		{
			final PaymentModeModel paymentMode = (PaymentModeModel) model;
			final Collection<AbstractOrderModel> paymentModeOrders = orderDao.findOrdersByPaymentMode(paymentMode);
			if (!paymentModeOrders.isEmpty())
			{
				throw new InterceptorException("Cannot remove paymentMode [" + paymentMode.getCode()
						+ "] as there are still orders using it!", this);
			}

		}
		else
		{
			Logger.getLogger(this.getClass()).warn("Unexpected model type!");
		}

	}

	@Required
	public void setOrderDao(final OrderDao orderDao)
	{
		this.orderDao = orderDao;
	}

}
