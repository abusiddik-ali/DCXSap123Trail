/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies;

import de.hybris.platform.core.enums.QuoteState;
import de.hybris.platform.core.model.order.QuoteModel;


/**
 * Strategy for creating a new snapshot of a {@link QuoteModel}.
 */
public interface CreateQuoteSnapshotStrategy
{

	/**
	 * Creates a new quote snapshot.
	 *
	 * @param quote      the {@link QuoteModel} to create a new snapshot for
	 * @param quoteState the desired {@link QuoteState} for the new quote snapshot
	 * @return the new quote snapshot
	 */
	QuoteModel createQuoteSnapshot(QuoteModel quote, QuoteState quoteState);
}
