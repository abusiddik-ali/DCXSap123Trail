/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.directpersistence.cache;

import java.io.Serializable;


public class TestClass implements Serializable
{
	public TestClass(final String name)
	{
		this.name = name;
	}

	private final String name;

	@Override
	public boolean equals(final Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (o == null || getClass() != o.getClass())
		{
			return false;
		}

		final TestClass testClass = (TestClass) o;

		if (name != null ? !name.equals(testClass.name) : testClass.name != null)
		{
			return false;
		}

		return true;
	}

	@Override
	public int hashCode()
	{
		return name != null ? name.hashCode() : 0;
	}
}
