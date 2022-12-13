/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies;

import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.QuoteModel;


/**
 * Strategy for creating a {@link QuoteModel} instance based on a given {@link CartModel} instance.
 */
public interface CreateQuoteFromCartStrategy
{

	/**
	 * Creates a new {@link QuoteModel} based on the given {@link CartModel}.
	 *
	 * @param cart the {@link CartModel} base on which to create a new {@link QuoteModel}
	 * @return the new {@link QuoteModel}
	 */
	QuoteModel createQuoteFromCart(final CartModel cart);
}
