/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.OrderService;


public interface SubmitOrderStrategy
{
	/**
	 * Submits the order. One of strategies that is invoked by {@link OrderService#submitOrder(OrderModel)}. You can have
	 * your own implementation(s) there.
	 *
	 * @param order order to submit.
	 */
	void submitOrder(OrderModel order);
}
