/*
 * ----------------------------------------------------------------
 * --- WARNING: THIS FILE IS GENERATED AND WILL BE OVERWRITTEN!
 * --- Generated at 14-Dec-2022, 10:02:09 PM
 * ----------------------------------------------------------------
 *
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.event.events;

import java.io.Serializable;
import de.hybris.platform.servicelayer.event.events.AbstractEvent;

public  class AfterSessionAttributeChangeEvent  extends AbstractEvent 
{


	/** <i>Generated property</i> for <code>AfterSessionAttributeChangeEvent.attributeName</code> property defined at extension <code>core</code>. */
	
	private String attributeName;

	/** <i>Generated property</i> for <code>AfterSessionAttributeChangeEvent.value</code> property defined at extension <code>core</code>. */
	
	private Object value;
	
	public AfterSessionAttributeChangeEvent()
	{
		super();
	}

	public AfterSessionAttributeChangeEvent(final Serializable source)
	{
		super(source);
	}
	
	public void setAttributeName(final String attributeName)
	{
		this.attributeName = attributeName;
	}

	public String getAttributeName() 
	{
		return attributeName;
	}
	
	public void setValue(final Object value)
	{
		this.value = value;
	}

	public Object getValue() 
	{
		return value;
	}
	


}
