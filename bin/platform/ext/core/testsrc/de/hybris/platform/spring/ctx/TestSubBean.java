/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.spring.ctx;

public class TestSubBean
{

	private String simpleProperty;

	/**
	 * @param simpleProperty the simpleProperty to set
	 */
	public void setSimpleProperty(final String simpleProperty)
	{
		this.simpleProperty = simpleProperty;
	}


	/**
	 * @return the simpleProperty
	 */
	public String getSimpleProperty()
	{
		return simpleProperty;
	}

}
