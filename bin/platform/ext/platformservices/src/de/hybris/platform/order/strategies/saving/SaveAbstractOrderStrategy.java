/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.saving;

/**
 * Strategy for saving order and order entries. Should define a logic that saves the order and order entries and
 * returned refreshed order model. Such strategy may be usefull for some special cases where you need a special routine
 * in order to handle order entries save. I.e if you need to save an order/cart entry at a specifiv entryNumber
 * position.
 */
public interface SaveAbstractOrderStrategy<O>
{
	/**
	 * Saves the given order model and the order entries.
	 *
	 * @param order order model
	 * @return saved and refreshed order model
	 */
	O saveOrder(final O order);
}
