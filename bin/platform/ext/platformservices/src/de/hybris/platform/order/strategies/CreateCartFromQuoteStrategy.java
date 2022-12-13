/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies;

import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.QuoteModel;


/**
 * Strategy for creating a {@link CartModel} instance based on a given {@link QuoteModel} instance.
 */
public interface CreateCartFromQuoteStrategy
{

	/**
	 * Creates a new {@link CartModel} based on the given {@link QuoteModel}.
	 *
	 * @param quote the {@link QuoteModel} base on which to create a new {@link CartModel}
	 * @return the new {@link CartModel}
	 */
	CartModel createCartFromQuote(QuoteModel quote);
}
