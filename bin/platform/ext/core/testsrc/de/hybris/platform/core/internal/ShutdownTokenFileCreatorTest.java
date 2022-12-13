/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core.internal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.util.Token;
import de.hybris.platform.util.config.ConfigIntf;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class ShutdownTokenFileCreatorTest
{
	private final Token token = Token.generateNew();

	@Mock
	private ConfigIntf config;

	@Test
	public void shouldGenerateTokenForSSLConnector()
	{
		final ShutdownTokenFileCreator creator = new ShutdownTokenFileCreator(token, config);

		when(config.getBoolean(ShutdownTokenFileCreator.TOMCAT_HTTP_CONNECTOR_SECURE, false)).thenReturn(false);
		when(config.getParameter(ShutdownTokenFileCreator.TOMCAT_SSL_PORT)).thenReturn("9002");

		final String shutdownTokenContent = creator.createShutdownTokenContent();

		assertThat(shutdownTokenContent).isEqualTo("https://localhost:9002/monitoring/suspendresume/halt;" + token.stringValue());
	}

	@Test
	public void shouldGenerateTokenForSecureHttpConnector()
	{
		final ShutdownTokenFileCreator creator = new ShutdownTokenFileCreator(token, config);

		when(config.getBoolean(ShutdownTokenFileCreator.TOMCAT_HTTP_CONNECTOR_SECURE, false)).thenReturn(true);
		when(config.getParameter(ShutdownTokenFileCreator.TOMCAT_HTTP_PORT)).thenReturn("9001");

		final String shutdownTokenContent = creator.createShutdownTokenContent();

		assertThat(shutdownTokenContent).isEqualTo("http://localhost:9001/monitoring/suspendresume/halt;" + token.stringValue());
	}

}
