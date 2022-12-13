/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

import de.hybris.platform.core.enums.QuoteState;
import de.hybris.platform.core.model.order.QuoteEntryModel;
import de.hybris.platform.core.model.order.QuoteModel;
import de.hybris.platform.order.strategies.CreateQuoteSnapshotStrategy;

import java.util.Optional;


/**
 * The Class DefaultCreateQuoteSnapshotStrategy.
 */
public class DefaultCreateQuoteSnapshotStrategy
		extends GenericAbstractOrderCloningStrategy<QuoteModel, QuoteEntryModel, QuoteModel>
		implements CreateQuoteSnapshotStrategy
{
	public DefaultCreateQuoteSnapshotStrategy()
	{
		super(QuoteModel.class, QuoteEntryModel.class, QuoteModel.class);
	}

	@Override
	public QuoteModel createQuoteSnapshot(final QuoteModel quote, final QuoteState quoteState)
	{
		validateParameterNotNullStandardMessage("quote", quote);
		validateParameterNotNullStandardMessage("quoteState", quoteState);

		final QuoteModel quoteSnapshot = clone(quote, Optional.of(quote.getCode()));
		quoteSnapshot.setState(quoteState);
		quoteSnapshot.setVersion(Integer.valueOf(quote.getVersion().intValue() + 1));

		postProcess(quote, quoteSnapshot);

		return quoteSnapshot;
	}
}
