/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util.logging.context;

import java.io.Closeable;
import java.util.Map;

/**
 * This handler does nothing
 */
public class NOPLoggingContextHandler implements LoggingContextHandler
{


	@Override
	public void put(final String key, final String val)
	{
		// does nothing
	}

	@Override
	public Closeable putCloseable(final String key,final String val){
		return null;
	}

	@Override
	public String get(final String key)
	{
		return null;
	}

	@Override
	public void remove(final String key)
	{
		// does nothing
	}

	@Override
	public void clear()
	{
		// does nothing
	}

	@Override
	public Map<String, String> getCopyOfContextMap()
	{
		return null;
	}

	@Override
	public void setContextMap(final Map<String, String> contextMap)
	{
		// does nothing
	}
}
