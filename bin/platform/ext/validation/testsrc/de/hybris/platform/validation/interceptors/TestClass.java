/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.validation.interceptors;

import java.math.BigDecimal;
import java.math.BigInteger;


public class TestClass
{
	private String stringPropertyNoAccessor;
	private String stringProperty;
	private boolean smallBooleanProperty;
	private Boolean bigBooleanProperty;
	private BigDecimal bigDecimalProperty;
	private BigInteger bigIntegerProperty;
	private Byte byteProperty;

	public String getStringProperty()
	{
		return stringProperty;
	}

	public boolean isSmallBooleanProperty()
	{
		return smallBooleanProperty;
	}

	public Boolean getBigBooleanProperty()
	{
		return bigBooleanProperty;
	}

	public BigDecimal getBigDecimalProperty()
	{
		return bigDecimalProperty;
	}

	public BigInteger getBigIntegerProperty()
	{
		return bigIntegerProperty;
	}

	public Byte getByteProperty()
	{
		return byteProperty;
	}
}
