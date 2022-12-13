/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

import de.hybris.platform.core.model.order.CartEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.QuoteModel;
import de.hybris.platform.order.strategies.CreateCartFromQuoteStrategy;

import java.util.Optional;


/**
 * Default implementation of {@link CreateCartFromQuoteStrategy}
 */
public class DefaultCreateCartFromQuoteStrategy extends GenericAbstractOrderCloningStrategy<CartModel, CartEntryModel, QuoteModel>
		implements CreateCartFromQuoteStrategy
{
	public DefaultCreateCartFromQuoteStrategy()
	{
		super(CartModel.class, CartEntryModel.class, QuoteModel.class);
	}

	@Override
	public CartModel createCartFromQuote(final QuoteModel quote)
	{
		validateParameterNotNullStandardMessage("quote", quote);

		final CartModel cart = clone(quote, Optional.empty());

		postProcess(quote, cart);

		return cart;
	}
}
