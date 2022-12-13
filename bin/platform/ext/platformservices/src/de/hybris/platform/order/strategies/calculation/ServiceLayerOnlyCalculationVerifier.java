/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation;

/**
 * An interface for marking service layer only calculation strategies.
 */
public interface ServiceLayerOnlyCalculationVerifier
{

	/**
	 * Implement this and return true if you are using only service-layer models
	 *
	 * @return true, if only service-layer models are used
	 */
	default boolean isSLOnly()
	{
		return false;
	}
}
