/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt.converter.impl;

import de.hybris.platform.europe1.model.PriceRowModel;
import de.hybris.platform.order.strategies.calculation.pdt.converter.PDTConverter;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.PriceValueInfoCriteria;
import de.hybris.platform.util.PriceValue;


public class DefaultPriceValueConverter implements PDTConverter<PriceRowModel, PriceValue, PriceValueInfoCriteria>
{

	@Override
	public PriceValue convert(final PriceRowModel priceRowModel, final PriceValueInfoCriteria criteria)
	{
		return new PriceValue(priceRowModel.getCurrency().getIsocode(), priceRowModel.getPrice(), priceRowModel.getNet());
	}

}
