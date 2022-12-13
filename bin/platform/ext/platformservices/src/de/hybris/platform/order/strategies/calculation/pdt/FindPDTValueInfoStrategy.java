/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt;

import de.hybris.platform.jalo.order.price.PDTInformation;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.PDTCriteria;
import de.hybris.platform.util.PDTValue;

import java.util.List;


/**
 * The Interface for find for price/discount/tax values or information.
 *
 * @param <VALUE>    the generic type for price/discount/tax values.
 * @param <INFO>     the generic type for price/discount/tax information.
 * @param <CRITERIA> the generic type for price/discount/tax criteria.
 */
public interface FindPDTValueInfoStrategy<VALUE extends PDTValue, INFO extends PDTInformation, CRITERIA extends PDTCriteria>
{

	/**
	 * Gets the price/discount/tax values.
	 *
	 * @param criteria the price/discount/tax criteria.
	 * @return the price/discount/tax values.
	 * @throws CalculationException the calculation exception
	 */
	List<VALUE> getPDTValues(CRITERIA criteria) throws CalculationException;

	/**
	 * Gets the price/discount/tax information.
	 *
	 * @param criteria the price/discount/tax criteria.
	 * @return the price/discount/tax information.
	 * @throws CalculationException the calculation exception
	 */
	List<INFO> getPDTInformation(CRITERIA criteria) throws CalculationException;
}
