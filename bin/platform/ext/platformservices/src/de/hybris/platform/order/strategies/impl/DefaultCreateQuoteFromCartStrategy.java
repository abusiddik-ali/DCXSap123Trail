/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.QuoteEntryModel;
import de.hybris.platform.core.model.order.QuoteModel;
import de.hybris.platform.order.strategies.CreateQuoteFromCartStrategy;

import java.util.Optional;


/**
 * Default implementation for {@link CreateQuoteFromCartStrategy}
 */
public class DefaultCreateQuoteFromCartStrategy
		extends GenericAbstractOrderCloningStrategy<QuoteModel, QuoteEntryModel, CartModel> implements CreateQuoteFromCartStrategy
{
	public DefaultCreateQuoteFromCartStrategy()
	{
		super(QuoteModel.class, QuoteEntryModel.class, CartModel.class);
	}

	@Override
	public QuoteModel createQuoteFromCart(final CartModel cart)
	{
		validateParameterNotNullStandardMessage("cart", cart);

		final QuoteModel quote = clone(cart, Optional.empty());

		postProcess(cart, quote);

		return quote;
	}
}
