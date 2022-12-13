/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;


/**
 * Strategy answers if order or order entry requires calculation
 */
public interface OrderRequiresCalculationStrategy
{

	/**
	 * Method checks if the order need to be calculated.
	 *
	 * @param order {@link AbstractOrderModel} to check
	 * @return <code>true</code> if order requires calculation
	 */
	boolean requiresCalculation(AbstractOrderModel order);

	/**
	 * Method checks if the order entry need to be calculated.
	 *
	 * @param orderEntry {@link AbstractOrderEntryModel} to check
	 * @return <code>true</code> if order entry requires calculation
	 */
	boolean requiresCalculation(AbstractOrderEntryModel orderEntry);
}
