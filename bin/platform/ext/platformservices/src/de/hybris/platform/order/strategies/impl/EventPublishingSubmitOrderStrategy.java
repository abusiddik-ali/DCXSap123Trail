/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.impl;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.order.events.SubmitOrderEvent;
import de.hybris.platform.order.strategies.SubmitOrderStrategy;
import de.hybris.platform.servicelayer.event.EventService;

import org.springframework.beans.factory.annotation.Required;


/**
 * This implementation sends {@link SubmitOrderEvent} event when order is submitted.
 */
public class EventPublishingSubmitOrderStrategy implements SubmitOrderStrategy
{
	private EventService eventService;

	@Override
	public void submitOrder(final OrderModel order)
	{
		eventService.publishEvent(new SubmitOrderEvent(order));
	}

	@Required
	public void setEventService(final EventService eventService)
	{
		this.eventService = eventService;
	}

}
