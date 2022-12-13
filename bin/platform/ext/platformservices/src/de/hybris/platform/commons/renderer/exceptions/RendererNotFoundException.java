/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.commons.renderer.exceptions;

/**
 * Exception thrown when no one renderer was found
 */
public class RendererNotFoundException extends RuntimeException
{
	public RendererNotFoundException(final String message)
	{
		super(message);
	}
}
