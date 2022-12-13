/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.scripting.events;

import de.hybris.platform.servicelayer.event.events.AbstractEvent;


/**
 * Test event for scripted event listeners
 */
public class TestScriptingEvent extends AbstractEvent
{
	private final String eventName;

	public TestScriptingEvent(final String eventName)
	{
		super();
		this.eventName = eventName;
	}

	public String getEventName()
	{
		return eventName;
	}

}
