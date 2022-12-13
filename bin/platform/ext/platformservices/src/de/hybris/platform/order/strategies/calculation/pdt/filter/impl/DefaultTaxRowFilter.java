/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt.filter.impl;

import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.europe1.model.TaxRowModel;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.TaxValueInfoCriteria;
import de.hybris.platform.order.strategies.calculation.pdt.filter.PDTRowFilter;
import de.hybris.platform.util.DateRange;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.ListIterator;


public class DefaultTaxRowFilter implements PDTRowFilter<TaxValueInfoCriteria, TaxRowModel>
{
	@Override
	public Collection<TaxRowModel> filter(final Collection<TaxRowModel> inputCollection, final TaxValueInfoCriteria criteria)
	{
		final Collection<TaxRowModel> collection = preFilter(inputCollection, criteria);

		if (collection.isEmpty())
		{
			return Collections.emptyList();
		}
		else
		{
			final List<TaxRowModel> ret = new ArrayList<>(collection);

			for (final ListIterator<TaxRowModel> it = ret.listIterator(); it.hasNext(); )
			{
				final TaxRowModel taxRow = it.next();

				final DateRange dateRange = taxRow.getDateRange();
				if (dateRange != null && !dateRange.encloses(criteria.getDate()))
				{
					it.remove();
				}
			}
			return ret;
		}
	}


	protected Collection<TaxRowModel> preFilter(final Collection<TaxRowModel> collection, final TaxValueInfoCriteria criteria)
	{
		final Collection<TaxRowModel> results = new ArrayList<>();

		for (final TaxRowModel taxRow : collection)
		{
			final CatalogVersionModel taxRowCatalogVersion = taxRow.getCatalogVersion();
			if (taxRowCatalogVersion == null || taxRowCatalogVersion.equals(criteria.getProduct().getCatalogVersion()))
			{
				results.add(taxRow);
			}
		}
		return results;
	}
}
