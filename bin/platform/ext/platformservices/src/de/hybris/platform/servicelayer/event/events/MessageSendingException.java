/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.event.events;

/**
 * Thrown when error while sending exception.
 */
public class MessageSendingException extends RuntimeException
{

	public MessageSendingException()
	{
		super();
	}

	public MessageSendingException(final String message, final Throwable exception)
	{
		super(message, exception);
	}

	public MessageSendingException(final String message)
	{
		super(message);
	}

	public MessageSendingException(final Throwable exception)
	{
		super(exception);
	}


}
