/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.masterserver.collector.system.impl;

import java.nio.charset.Charset;
import java.util.Base64;
import java.util.zip.DataFormatException;
import java.util.zip.Inflater;

final class SpringOverviewTestHelper
{
	private SpringOverviewTestHelper()
	{
		//no instantiation
	}

	static byte[] base64Decode(final String str)
	{
		return Base64.getDecoder().decode(str);
	}

	static String decompress(final byte[] compressedBytes, final Charset encoding)
	{
		final Inflater inflater = new Inflater();
		final byte[] buffer = new byte[compressedBytes.length * 20];
		inflater.setInput(compressedBytes);
		try
		{
			final int size = inflater.inflate(buffer);
			return new String(buffer, 0, size, encoding);
		}
		catch (final DataFormatException e)
		{
			throw new RuntimeException(e);
		}
	}
}
