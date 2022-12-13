/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies;

import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.order.InvalidCartException;


/**
 * A cart validator.
 *
 * @spring.bean cartValidator
 */
public interface CartValidator
{
	/**
	 * validates the {@link CartModel}. <b>Currently not in use!</b>
	 *
	 * @param cart the cart
	 * @throws InvalidCartException an exception
	 */
	void validateCart(CartModel cart) throws InvalidCartException;
}
