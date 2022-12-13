/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt.processor;

import de.hybris.platform.order.exceptions.CalculationException;


/**
 * Current <code>PDTProcessor</code> can use the <code>PDTProcessorChain</code> to invoke the next processor in the
 * chain
 */
public interface PDTProcessorChain
{
	/**
	 * Causes the next processor in the chain to be invoked
	 *
	 * @param context the context to pass along the chain
	 * @throws CalculationException the calculation exception
	 */
	void doProcess(PDTContext context) throws CalculationException;

}
