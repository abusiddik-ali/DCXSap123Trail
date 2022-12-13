/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.event;

import de.hybris.platform.servicelayer.event.MockEventServiceTest.CustomClusterEvent;
import de.hybris.platform.servicelayer.event.MockEventServiceTest.CustomEvent;
import de.hybris.platform.servicelayer.event.events.AbstractEvent;
import de.hybris.platform.servicelayer.event.impl.AbstractEventListener;


public class ConfiguredTestListener extends AbstractEventListener<AbstractEvent>
{
	public AbstractEvent lastEvent;


	public AbstractEvent getLastEvent()
	{
		return lastEvent;
	}


	@Override
	protected void onEvent(final AbstractEvent event)
	{
		if (event instanceof CustomEvent || event instanceof CustomClusterEvent)
		{
			this.lastEvent = event;
		}
	}
}
