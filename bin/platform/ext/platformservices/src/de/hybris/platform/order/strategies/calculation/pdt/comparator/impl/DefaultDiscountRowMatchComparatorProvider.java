/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt.comparator.impl;

import de.hybris.platform.europe1.model.AbstractDiscountRowModel;
import de.hybris.platform.order.strategies.calculation.pdt.comparator.PDTRowMatchComparatorProvider;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.DiscountValueInfoCriteria;

import java.util.Comparator;


public class DefaultDiscountRowMatchComparatorProvider
		implements PDTRowMatchComparatorProvider<DiscountValueInfoCriteria, AbstractDiscountRowModel>
{
	@Override
	public Comparator<AbstractDiscountRowModel> comparator(final DiscountValueInfoCriteria criteria)
	{
		return new DiscountRowMatchComparator();
	}

	protected static class DiscountRowMatchComparator implements Comparator<AbstractDiscountRowModel>
	{
		@Override
		public int compare(final AbstractDiscountRowModel row1, final AbstractDiscountRowModel row2)
		{
			// match value
			final int product1 = row1.getDiscount().getPriority().intValue();
			final int product2 = row2.getDiscount().getPriority().intValue();
			if (product1 != product2)
			{
				// DESC -> row is better if priority is greater !
				return product1 - product2;
			}

			// currency
			final boolean c1Set = row1.getCurrency() != null;
			final boolean c2Set = row2.getCurrency() != null;
			if (c1Set != c2Set)
			{
				// the rows having a currency are better
				return c1Set ? -1 : 1;
			}

			// sort by PK otherwise to have a stable order
			return row1.getPk().compareTo(row2.getPk());
		}
	}

}
