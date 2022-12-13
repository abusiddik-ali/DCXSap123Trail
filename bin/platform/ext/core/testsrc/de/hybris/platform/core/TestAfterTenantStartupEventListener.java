/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core;

import de.hybris.platform.servicelayer.event.events.AfterTenantRestartEvent;
import de.hybris.platform.servicelayer.event.impl.AbstractEventListener;

import java.util.ArrayList;
import java.util.List;


/**
 *
 */
public class TestAfterTenantStartupEventListener extends AbstractEventListener<AfterTenantRestartEvent>
{

	private final List<AfterTenantRestartEvent> eventsRecorded = new ArrayList<AfterTenantRestartEvent>();

	@Override
	protected void onEvent(final AfterTenantRestartEvent event)
	{
		eventsRecorded.add(event);
	}


	public List<AfterTenantRestartEvent> getEventsRecoreded()
	{
		return eventsRecorded;
	}
}
