/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.util.PriceValue;


/**
 * Strategy focused on resolving payment cost for a given order. Payment cost depends on the payment mode chosen at the
 * checkout.
 */
public interface FindPaymentCostStrategy extends ServiceLayerOnlyCalculationVerifier
{

	/**
	 * Returns cost of the given order that is related with payment.
	 *
	 * @param order {@link AbstractOrderModel}
	 * @return {@link PriceValue} representing payment cost introduced in the order.
	 */
	PriceValue getPaymentCost(AbstractOrderModel order);
}
