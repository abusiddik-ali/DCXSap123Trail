/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.converters.impl;

import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.dto.converter.Converter;

import java.util.concurrent.atomic.AtomicInteger;

/**
 *
 */
public class DummyConverterForMockito implements Converter<AtomicInteger, AtomicInteger>
{

	@Override
	public AtomicInteger convert(AtomicInteger source) throws ConversionException
	{
		return new AtomicInteger(source.get() * -1);
	}

	@Override
	public AtomicInteger convert(AtomicInteger source, AtomicInteger prototype) throws ConversionException
	{
		prototype.set(source.get() * -1);
		return prototype;
	}

}
