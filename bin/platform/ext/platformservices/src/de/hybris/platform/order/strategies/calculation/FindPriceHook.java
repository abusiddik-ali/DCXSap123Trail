/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.order.strategies.calculation.impl.servicelayer.DefaultSLFindPriceStrategy;
import de.hybris.platform.util.PriceValue;


/**
 * Hook that focuses on resolving {@link PriceValue} for the given {@link AbstractOrderEntryModel}. Hook implementations
 * need to be registered with {@link DefaultSLFindPriceStrategy}
 */
public interface FindPriceHook extends ServiceLayerOnlyCalculationVerifier
{

	/**
	 * Resolves custom price value for the given {@link AbstractOrderEntryModel}.
	 *
	 * @param entry
	 *           {@link AbstractOrderEntryModel}
	 * @param defaultPrive
	 *           default {@link PriceValue}
	 *
	 * @return {@link PriceValue}
	 */
	PriceValue findCustomBasePrice(AbstractOrderEntryModel entry, PriceValue defaultPrice);

	/**
	 * Indicates whether a custom base price can be found for the given {@link AbstractOrderEntryModel}.
	 *
	 * @param entry
	 *           {@link AbstractOrderEntryModel}
	 * @return whether hook is applicable
	 */
	boolean isApplicable(AbstractOrderEntryModel entry);

}
