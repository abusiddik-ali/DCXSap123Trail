/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order;

import de.hybris.platform.servicelayer.exceptions.BusinessException;


/**
 * not used!
 *
 * @deprecated since ages
 */
@Deprecated(since = "ages", forRemoval = true)
public class PriceException extends BusinessException
{
	public PriceException(final String message)
	{
		super(message);
	}

	public PriceException(final Throwable cause)
	{
		super(cause);
	}

	public PriceException(final String message, final Throwable cause)
	{
		super(message, cause);
	}

}
