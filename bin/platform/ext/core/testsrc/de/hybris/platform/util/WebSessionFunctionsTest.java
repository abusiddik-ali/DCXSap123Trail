/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Constants;
import de.hybris.platform.core.Registry;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import javax.servlet.http.HttpSession;

import org.junit.Test;

@IntegrationTest
public class WebSessionFunctionsTest extends ServicelayerBaseTest
{
	@Test
	public void shouldReturnNullSessionIfThereIsNoTenant() throws Exception
	{
		final HttpSession sessionMock = mock(HttpSession.class);
		when(sessionMock.getAttribute(Constants.WEB.JALOSESSION)).thenReturn(mock(JaloSession.class));

		Registry.runAsTenant(null, () -> {

			final JaloSession session = WebSessionFunctions.tryGetJaloSession(sessionMock);

			assertThat(session).isNull();

			return null;
		});

		verify(sessionMock, times(1)).getAttribute(Constants.WEB.JALOSESSION);
		verifyNoMoreInteractions(sessionMock);
	}
}