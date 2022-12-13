/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.web.session.stale.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.web.session.stale.StaleSessionStrategy;
import de.hybris.platform.util.WebSessionFunctions;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.junit.Test;

@IntegrationTest
public class InvalidateSessionAndMarkRequestStrategyTest extends ServicelayerBaseTest
{
	@Test
	public void shouldMarkRequestAndInvalidateSession()
	{
		final InvalidateSessionAndMarkRequestStrategy strategy = new InvalidateSessionAndMarkRequestStrategy();

		final HttpSession session = mock(HttpSession.class);
		final HttpServletRequest request = mock(RequestMock.class, withSettings().useConstructor());
		final HttpServletResponse response = mock(HttpServletResponse.class);

		doCallRealMethod().when(request).getAttribute(anyString());
		doCallRealMethod().when(request).setAttribute(anyString(), anyObject());
		when(request.getSession()).thenReturn(session);

		assertThat(WebSessionFunctions.isStaleRequest(request)).isFalse();

		final StaleSessionStrategy.Action action = strategy.onStaleSession(
				request, response);

		assertThat(action).isNotNull().isSameAs(StaleSessionStrategy.Action.CONTINUE_REQUEST_PROCESSING);
		assertThat(WebSessionFunctions.isStaleRequest(request)).isTrue();
		verify(session, times(1)).invalidate();

	}

	public static abstract class RequestMock implements HttpServletRequest
	{
		private final Map<String, Object> arguments = new HashMap<>();

		@Override
		public void setAttribute(final String name, final Object value)
		{
			arguments.put(name, value);
		}

		@Override
		public Object getAttribute(final String name)
		{
			return arguments.get(name);
		}
	}

}