/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.validation.events;

import static org.junit.Assert.assertSame;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.event.AbstractAsynchronousEventServiceTest;
import de.hybris.platform.servicelayer.event.events.AfterInitializationEndEvent;

import org.junit.Test;


@IntegrationTest
public class CustomEventListenerIntegrationTest extends AbstractAsynchronousEventServiceTest
{
	@Test
	public void testInistializationEndMessageSend() throws Exception
	{
		setExpectedEventClass(AfterInitializationEndEvent.class);
		final AfterInitializationEndEvent event = new AfterInitializationEndEvent();
		eventService.publishEvent(event);

		assertSame("Received event is not expected one.", event, pollEvent());
	}
}
