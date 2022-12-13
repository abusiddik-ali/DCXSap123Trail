/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order;

import de.hybris.platform.servicelayer.exceptions.BusinessException;


/**
 * Thrown when creation or modification of cart is not possible.
 */
public class InvalidCartException extends BusinessException
{

	public InvalidCartException(final String message, final Throwable cause)
	{
		super(message, cause);
	}

	public InvalidCartException(final String message)
	{
		super(message);
	}

	public InvalidCartException(final Throwable cause)
	{
		super(cause);
	}

}
