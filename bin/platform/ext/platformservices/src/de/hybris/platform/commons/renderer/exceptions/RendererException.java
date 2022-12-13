/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.commons.renderer.exceptions;

/**
 * Exception thrown where error during rendering occurr
 */
public class RendererException extends RuntimeException
{
	public RendererException(final String message)
	{
		super(message);
	}

	public RendererException(final String message, final Throwable throwable)
	{
		super(message, throwable);
	}
}
