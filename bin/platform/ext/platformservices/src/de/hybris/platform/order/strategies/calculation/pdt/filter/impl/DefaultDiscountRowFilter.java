/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt.filter.impl;

import de.hybris.platform.europe1.model.AbstractDiscountRowModel;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.DiscountValueInfoCriteria;
import de.hybris.platform.order.strategies.calculation.pdt.filter.PDTRowFilter;
import de.hybris.platform.util.DateRange;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.ListIterator;


public class DefaultDiscountRowFilter implements PDTRowFilter<DiscountValueInfoCriteria, AbstractDiscountRowModel>
{
	@Override
	public Collection<AbstractDiscountRowModel> filter(final Collection<AbstractDiscountRowModel> collection,
	                                                   final DiscountValueInfoCriteria criteria)
	{
		if (collection.isEmpty())
		{
			return Collections.emptyList();
		}

		final List<AbstractDiscountRowModel> ret = new ArrayList<>(collection);

		for (final ListIterator<AbstractDiscountRowModel> it = ret.listIterator(); it.hasNext(); )
		{
			final AbstractDiscountRowModel abstractDiscountRow = it.next();

			final DateRange dataRange = abstractDiscountRow.getDateRange();
			if (dataRange != null && !dataRange.encloses(criteria.getDate()))
			{
				it.remove();
			}
		}
		return ret;

	}
}
