/*
 * ----------------------------------------------------------------
 * --- WARNING: THIS FILE IS GENERATED AND WILL BE OVERWRITTEN!
 * --- Generated at 16-Dec-2022, 6:42:56 PM
 * ----------------------------------------------------------------
 *
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.event.events;

import java.io.Serializable;
import de.hybris.platform.servicelayer.event.events.AbstractEvent;

import de.hybris.platform.util.JspContext;
import java.util.Map;

public  class AfterInitializationEndEvent  extends AbstractEvent 
{


	/** <i>Generated property</i> for <code>AfterInitializationEndEvent.params</code> property defined at extension <code>core</code>. */
	
	private Map<String, String> params;

	/** <i>Generated property</i> for <code>AfterInitializationEndEvent.ctx</code> property defined at extension <code>core</code>. */
	
	private JspContext ctx;
	
	public AfterInitializationEndEvent()
	{
		super();
	}

	public AfterInitializationEndEvent(final Serializable source)
	{
		super(source);
	}
	
	public void setParams(final Map<String, String> params)
	{
		this.params = params;
	}

	public Map<String, String> getParams() 
	{
		return params;
	}
	
	public void setCtx(final JspContext ctx)
	{
		this.ctx = ctx;
	}

	public JspContext getCtx() 
	{
		return ctx;
	}
	


}
