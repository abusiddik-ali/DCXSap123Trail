/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog;

/**
 * Value object representing count of duplicated item of given type.
 */
public class DuplicatedItemIdentifier
{
	private final String composedType;
	private final String code;
	private final int count;

	public DuplicatedItemIdentifier(final String composedType, final String code, final int count)
	{
		this.composedType = composedType;
		this.code = code;
		this.count = count;
	}

	public String getComposedType()
	{
		return composedType;
	}

	public String getCode()
	{
		return code;
	}

	public int getCount()
	{
		return count;
	}

	@Override
	public String toString()
	{
		return "DuplicatedItemIdentifier{" + "composedType='" + composedType + '\'' + ", code='" + code + '\'' + ", count=" + count
				+ '}';
	}
}
