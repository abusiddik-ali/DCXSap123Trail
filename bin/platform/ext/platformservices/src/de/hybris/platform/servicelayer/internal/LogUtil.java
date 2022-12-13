/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.internal;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.slf4j.LoggerFactory;


public class LogUtil
{
	private static final Set<Integer> LOGGED_MESSAGES = ConcurrentHashMap.newKeySet();

	private LogUtil()
	{
	}

	public static void infoOnce(final Class clazz, final Integer messageId, final String message)
	{
		if (LOGGED_MESSAGES.add(messageId))
		{
			LoggerFactory.getLogger(clazz).info(message);
		}
	}
}
