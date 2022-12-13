/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.licence.sap;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import com.sap.security.core.server.likey.Persistence;


/**
 * Test license persistence implementation.
 */
public class TestPersistence implements Persistence
{
	final Map<String, String> repository = new HashMap<>();

	@Override
	public boolean init()
	{
		return true;
	}

	@Override
	public boolean insertKey(final String key, final String value)
	{
		repository.put(key, value);
		return true;
	}

	@Override
	public boolean updateKey(final String key, final String value)
	{
		return insertKey(key, value);
	}

	@Override
	public boolean deleteKey(final String key)
	{
		repository.remove(key);
		return true;
	}

	@Override
	public String getKey(final String key)
	{
		return repository.get(key);
	}

	@Override
	public Properties getKeys()
	{
		final Properties properties = new Properties();
		for (final Map.Entry<String, String> entry : repository.entrySet())
		{
			properties.put(entry.getKey(), entry.getValue());
		}

		return properties;
	}
}
