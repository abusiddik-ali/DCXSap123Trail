/*
 * ----------------------------------------------------------------
 * --- WARNING: THIS FILE IS GENERATED AND WILL BE OVERWRITTEN!
 * --- Generated at 14-Dec-2022, 9:23:34 PM
 * ----------------------------------------------------------------
 *
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.event.events;

import java.io.Serializable;
import de.hybris.platform.servicelayer.event.events.AbstractEvent;

public  class InvalidateModelConverterRegistryEvent  extends AbstractEvent 
{


	/** <i>Generated property</i> for <code>InvalidateModelConverterRegistryEvent.composedTypeCode</code> property defined at extension <code>core</code>. */
	
	private String composedTypeCode;

	/** <i>Generated property</i> for <code>InvalidateModelConverterRegistryEvent.composedClass</code> property defined at extension <code>core</code>. */
	
	private Class composedClass;

	/** <i>Generated property</i> for <code>InvalidateModelConverterRegistryEvent.refresh</code> property defined at extension <code>core</code>. */
	
	private boolean refresh;
	
	public InvalidateModelConverterRegistryEvent()
	{
		super();
	}

	public InvalidateModelConverterRegistryEvent(final Serializable source)
	{
		super(source);
	}
	
	public void setComposedTypeCode(final String composedTypeCode)
	{
		this.composedTypeCode = composedTypeCode;
	}

	public String getComposedTypeCode() 
	{
		return composedTypeCode;
	}
	
	public void setComposedClass(final Class composedClass)
	{
		this.composedClass = composedClass;
	}

	public Class getComposedClass() 
	{
		return composedClass;
	}
	
	public void setRefresh(final boolean refresh)
	{
		this.refresh = refresh;
	}

	public boolean isRefresh() 
	{
		return refresh;
	}
	


}
