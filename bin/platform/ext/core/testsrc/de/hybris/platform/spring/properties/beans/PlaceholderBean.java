/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.spring.properties.beans;

/**
 *
 */
public class PlaceholderBean
{
	private String value;

	public void setValue(final String value)
	{
		this.value = value;
	}

	public String getValue()
	{
		return value;
	}
}
