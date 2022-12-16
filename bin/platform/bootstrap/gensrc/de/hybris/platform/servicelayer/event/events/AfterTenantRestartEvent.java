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

public  class AfterTenantRestartEvent  extends AbstractEvent 
{


	/** <i>Generated property</i> for <code>AfterTenantRestartEvent.tenantId</code> property defined at extension <code>core</code>. */
	
	private String tenantId;
	
	public AfterTenantRestartEvent()
	{
		super();
	}

	public AfterTenantRestartEvent(final Serializable source)
	{
		super(source);
	}
	
	public void setTenantId(final String tenantId)
	{
		this.tenantId = tenantId;
	}

	public String getTenantId() 
	{
		return tenantId;
	}
	


}
