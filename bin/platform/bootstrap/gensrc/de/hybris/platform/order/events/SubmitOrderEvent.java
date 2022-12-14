/*
 * ----------------------------------------------------------------
 * --- WARNING: THIS FILE IS GENERATED AND WILL BE OVERWRITTEN!
 * --- Generated at 14-Dec-2022, 9:04:56 PM
 * ----------------------------------------------------------------
 *
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.events;

import java.io.Serializable;
import de.hybris.platform.servicelayer.event.events.AbstractEvent;

import de.hybris.platform.core.model.order.OrderModel;

public class SubmitOrderEvent  extends AbstractEvent 
{

	/** <i>Generated property</i> for <code>SubmitOrderEvent.order</code> property defined at extension <code>platformservices</code>. */
	private OrderModel order;
	
	public SubmitOrderEvent()
	{
		super();
	}

	/**
	 * Attention: for backward compatibility this constructor invokes <pre>setOrder(source)</pre> in case the source object is a OrderModel!  
	 */
	public SubmitOrderEvent(final Serializable source)
	{
		super(source);
		
		// compatibility!
		if( source instanceof OrderModel )
		{
			setOrder((OrderModel)source);
		}
	}
	
	
	public void setOrder(final OrderModel order)
	{
		this.order = order;
	}
	
	
	public OrderModel getOrder() 
	{
		return order;
	}
	
}