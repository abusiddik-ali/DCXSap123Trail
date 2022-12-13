/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.order;

import de.hybris.platform.directpersistence.annotation.ForceJALO;
import de.hybris.platform.jalo.order.Order;
import de.hybris.platform.jalo.order.price.JaloPriceFactoryException;
import de.hybris.platform.jalo.type.ComposedType;
import de.hybris.platform.util.PriceValue;

import java.util.Collections;
import java.util.List;


public class TestOrder extends Order
{

	@Override
	@ForceJALO(reason = ForceJALO.SOMETHING_ELSE)
	protected ComposedType getCustomEntryType()
	{
		return getSession().getTypeManager().getComposedType(TestOrderEntry.class);
	}

	@Override
	protected PriceValue findDeliveryCosts() throws JaloPriceFactoryException
	{
		return null;
	}

	@Override
	protected List findGlobalDiscounts() throws JaloPriceFactoryException
	{
		return Collections.EMPTY_LIST;
	}

	@Override
	protected PriceValue findPaymentCosts() throws JaloPriceFactoryException
	{
		return null;
	}
}
