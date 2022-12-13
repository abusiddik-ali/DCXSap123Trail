/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.scripting.events;

import de.hybris.platform.servicelayer.event.events.AbstractEvent;


/**
 * test event for performance tests
 */
public class TestPerformanceEvent extends AbstractEvent
{
	private final int itemsToSaveCount;

	public TestPerformanceEvent(final int itemsToSaveCount)
	{
		super();
		this.itemsToSaveCount = itemsToSaveCount;
	}

	public int getItemsToSaveCount()
	{
		return itemsToSaveCount;
	}
}
