/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.event.events;

/**
 * Testing event used only for testing current Spring Integration.
 */
public class TestingEvent extends AbstractEvent
{
	private final String message;

	public TestingEvent(final String message)
	{
		super();
		this.message = message;
	}

	public String getMessage()
	{
		return message;
	}
}
