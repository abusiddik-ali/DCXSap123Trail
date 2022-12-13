/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies;

import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.InvalidCartException;
import de.hybris.platform.order.OrderService;
import de.hybris.platform.order.strategies.ordercloning.CloneAbstractOrderStrategy;


/**
 * <p>
 * The strategy creates an {@link OrderModel} instance out of a given {@link CartModel} instance. The strategy is used
 * in {@link OrderService#createOrderFromCart(CartModel)} to introduce a new order. By default, the strategy validates
 * the cart using {@link CartValidator} and clones it as an order using configured {@link CloneAbstractOrderStrategy}.
 * </p>
 */
public interface CreateOrderFromCartStrategy
{
	/**
	 * Validates the cart using {@link CartValidator} and performs cart to order cloning.
	 *
	 * @param cart - the target {@link CartModel}
	 * @return an unsaved and not calculated {@link OrderModel} instance.
	 * @throws InvalidCartException according to {@link CartValidator} implementation.
	 */
	OrderModel createOrderFromCart(CartModel cart) throws InvalidCartException;
}
