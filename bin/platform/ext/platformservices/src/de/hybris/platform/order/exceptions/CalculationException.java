/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.exceptions;

import de.hybris.platform.order.CalculationService;
import de.hybris.platform.servicelayer.exceptions.BusinessException;


/**
 * A general exception used by {@link CalculationService} extensions if an (expected) error occurs during price
 * calculation or requesting price informations.
 */
public class CalculationException extends BusinessException
{

	public CalculationException(final String message)
	{
		super(message);
	}

	public CalculationException(final String message, final Throwable cause)
	{
		super(message, cause);
	}

	public CalculationException(final Throwable cause)
	{
		super(cause);
	}

}
