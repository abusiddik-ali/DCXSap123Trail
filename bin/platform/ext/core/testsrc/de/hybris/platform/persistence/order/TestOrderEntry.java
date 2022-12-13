/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.order;

import de.hybris.platform.jalo.order.OrderEntry;
import de.hybris.platform.jalo.order.price.JaloPriceFactoryException;
import de.hybris.platform.util.PriceValue;

import java.util.Collection;
import java.util.Collections;
import java.util.List;


public class TestOrderEntry extends OrderEntry
{
	public static final double PRICE = 123.45;


	@Override
	protected List findDiscounts() throws JaloPriceFactoryException
	{
		return Collections.EMPTY_LIST;
	}

	@Override
	protected PriceValue findPrice() throws JaloPriceFactoryException
	{
		return new PriceValue(getOrder().getCurrency().getIsoCode(), PRICE, getOrder().isNet().booleanValue());
	}

	@Override
	protected Collection findTaxes() throws JaloPriceFactoryException
	{
		return Collections.EMPTY_LIST;
	}
}
