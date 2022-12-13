/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.i18n;

import de.hybris.platform.servicelayer.i18n.impl.DefaultI18NService;

import java.util.Locale;
import java.util.ResourceBundle;
import java.util.TimeZone;


public class I18NServiceMock extends DefaultI18NService
{
	@Override
	public ResourceBundle getBundle(final String baseName)
	{
		throw new UnsupportedOperationException();
	}

	@Override
	public ResourceBundle getBundle(final String baseName, final Locale[] locales)
	{
		throw new UnsupportedOperationException();
	}

	@Override
	public ResourceBundle getBundle(final String baseName, final Locale[] locales, final ClassLoader loader)
	{
		throw new UnsupportedOperationException();
	}

	@Override
	public void setCurrentTimeZone(final TimeZone zone)
	{
		throw new UnsupportedOperationException();
	}

	@Override
	public TimeZone getCurrentTimeZone()
	{
		throw new UnsupportedOperationException();
	}


}
