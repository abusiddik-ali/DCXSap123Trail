/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.util.DiscountValue;

import java.util.List;

/**
 * Hook that focuses on resolving {@link DiscountValue} for the given {@link AbstractOrderEntryModel}
 */
public interface FindDiscountValuesHook
{
	/**
	 * Find applicable {@link DiscountValue}s for the target order entry.
	 *
	 * @param entry
	 * @return List of {@link DiscountValue}s
	 */
	List<DiscountValue> findDiscountValues(AbstractOrderEntryModel entry);

	/**
	 * Indicates whether a custom base price can be found for the given {@link AbstractOrderEntryModel}.
	 *
	 * @param entry {@link AbstractOrderEntryModel}
	 * @return whether hook is applicable
	 */
	boolean isApplicable(AbstractOrderEntryModel entry);
}