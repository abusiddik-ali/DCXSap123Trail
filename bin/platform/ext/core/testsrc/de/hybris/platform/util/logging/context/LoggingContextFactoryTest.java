/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util.logging.context;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.util.Utilities;

import java.io.Closeable;
import java.io.IOException;

import org.junit.Test;

@IntegrationTest
public class LoggingContextFactoryTest extends ServicelayerBaseTest
{


	@Test
	public void shouldSwitchToNOPMDCHandler()
	{

		final String getActualMDCHandler = Utilities.getConfig()
		                                            .getString(LoggingContextFactory.CONFIG_LOGGINGCONTEXT_HANDLER_CLASS, "");

		try
		{
			Utilities.getConfig()
			         .setParameter(LoggingContextFactory.CONFIG_LOGGINGCONTEXT_HANDLER_CLASS, NOPLoggingContextHandler.class.getName());
			LoggingContextFactory.reset();

			final LoggingContextHandler handler = LoggingContextFactory.getLoggingContextHandler();
			handler.put("someKey", "someValue");

			assertThat(handler.get("someKey")).isNull();
		}
		finally
		{
			Utilities.getConfig().setParameter(LoggingContextFactory.CONFIG_LOGGINGCONTEXT_HANDLER_CLASS, getActualMDCHandler);
			LoggingContextFactory.reset();
		}
	}

	@Test
	public void shouldUseFallbackMDCHandlerWhenConfigIsNotSet()
	{

		final String getActualMDCHandler = Utilities.getConfig()
		                                            .getString(LoggingContextFactory.CONFIG_LOGGINGCONTEXT_HANDLER_CLASS, "");

		try
		{
			Utilities.getConfig().setParameter(LoggingContextFactory.CONFIG_LOGGINGCONTEXT_HANDLER_CLASS, "");

			LoggingContextFactory.reset();

			final LoggingContextHandler handler = LoggingContextFactory.getLoggingContextHandler();
			handler.put("someKey", "someValue");

			assertThat(handler).isEqualTo(LoggingContextFactory.getDefaultLoggingContextHandler());
			assertThat(handler.get("someKey")).isEqualTo("someValue");
		}
		finally
		{
			Utilities.getConfig().setParameter(LoggingContextFactory.CONFIG_LOGGINGCONTEXT_HANDLER_CLASS, getActualMDCHandler);
			LoggingContextFactory.reset();
		}

	}

	@Test
	public void shouldUseCloseable() throws IOException
	{
		final LoggingContextHandler handler = LoggingContextFactory.getLoggingContextHandler();
		try (final Closeable closable = LoggingContextFactory.getLoggingContextHandler().putCloseable("someKey", "someValue"))
		{
			assertThat(handler.get("someKey")).isEqualTo("someValue");
		}

		assertThat(handler.get("someKey")).isNull();
	}
}
