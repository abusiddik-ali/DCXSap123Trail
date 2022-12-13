/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt.filter;

import de.hybris.platform.europe1.model.PDTRowModel;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.PDTCriteria;

import java.util.Collection;


/**
 * The Interface for filtering price/discount/tax rows models using criteria.
 *
 * @param <CRITERIA> the generic type for price/discount/tax criteria.
 * @param <MODEL>    the generic type for price/discount/tax row models.
 */
public interface PDTRowFilter<CRITERIA extends PDTCriteria, MODEL extends PDTRowModel>
{

	/**
	 * Filters price/discount/tax models using given criteria.
	 *
	 * @param collection the price/discount/tax collection to filter.
	 * @param criteria   the price/discount/tax criteria.
	 * @return the filtered price/discount/tax models collection.
	 */
	Collection<MODEL> filter(Collection<MODEL> collection, CRITERIA criteria);
}
