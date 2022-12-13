/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt.postmatcher;

import de.hybris.platform.europe1.model.PDTRowModel;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.PDTCriteria;
import de.hybris.platform.order.strategies.calculation.pdt.impl.GenericPDTFindValueInfoStrategy;
import de.hybris.platform.order.strategies.calculation.pdt.matcher.PDTModelMatcher;

import java.util.Collection;


/**
 * The Interface for adding custom logic after invoking the {@link PDTModelMatcher} in the
 * {@link GenericPDTFindValueInfoStrategy}.
 *
 * @param <CRITERIA> the generic type for price/discount/tax criteria.
 * @param <MODEL>    the generic type for price/discount/tax model.
 */
public interface PDTPostMatcher<CRITERIA extends PDTCriteria, MODEL extends PDTRowModel>
{

	/**
	 * Process the custom post matcher logic.
	 *
	 * @param models   the collection of price/discount/tax models
	 * @param criteria the price/discount/tax criteria
	 * @return the processed price/discount/tax models collection
	 * @throws CalculationException the calculation exception
	 */
	Collection<MODEL> process(Collection<MODEL> models, CRITERIA criteria) throws CalculationException;
}
