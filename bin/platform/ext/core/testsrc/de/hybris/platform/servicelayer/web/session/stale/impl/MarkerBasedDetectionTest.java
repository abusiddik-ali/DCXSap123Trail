/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.web.session.stale.impl;

import static de.hybris.platform.core.Constants.USER.ANONYMOUS_CUSTOMER;
import static de.hybris.platform.servicelayer.web.session.stale.impl.MarkerBasedDetection.STALE_SESSION_MARKER;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.servicelayer.user.listener.PasswordChangeEvent;

import org.junit.Test;

@UnitTest
public class MarkerBasedDetectionTest
{
	private final String ANONYMOUS_USER_ID = ANONYMOUS_CUSTOMER;
	private final String TOKEN = null;
	private final String SOME_USER_ID = "some.user";
	private final String SOME_PASSWORD = "some.password";
	private final String SOME_PASSWORD_MARKER = MarkerBasedDetection.calculateMarker(SOME_PASSWORD + "_" + TOKEN);

	private final String ANOTHER_USER_ID = "another.user";
	private final String ANOTHER_PASSWORD = "another.password";
	private final String ANOTHER_PASSWORD_MARKER = MarkerBasedDetection.calculateMarker(ANOTHER_PASSWORD + "_" + TOKEN);

	private final String NULL_PASSWORD_MARKER = MarkerBasedDetection.calculateMarker(null + "_" + TOKEN);

	@Test
	public void shouldNotModifyMarkerForAnonymousSessionWhenMarkerWasNotSet()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", ANONYMOUS_USER_ID, null, TOKEN);

		try (final MarkerBasedDetection detection = new MarkerBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
		}

		verify(sessionContext, never()).setSessionAttribute(any(), any());
	}

	@Test
	public void shouldClearMarkerForAnonymousSessionWhenMarkerWasSet()
	{
		final SessionContext sessionContext = mockSessionContext("S_4", ANONYMOUS_USER_ID, null, "M");

		try (final MarkerBasedDetection detection = new MarkerBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
		}

		verify(sessionContext, times(1)).setSessionAttribute(STALE_SESSION_MARKER, null);
	}

	@Test
	public void shouldClearMarkerForNotAnonymousSessionAfterUserChangedToAnonymous()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", SOME_USER_ID, SOME_PASSWORD, SOME_PASSWORD_MARKER);

		try (final MarkerBasedDetection detection = new MarkerBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
			switchSessionContextMockTo(sessionContext, "S_1", ANONYMOUS_USER_ID, null, SOME_PASSWORD_MARKER);
		}

		verify(sessionContext, times(1)).setSessionAttribute(STALE_SESSION_MARKER, null);
	}

	@Test
	public void shouldSetMarkerForNotAnonymousSessionAfterSessionChanged()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", SOME_USER_ID, SOME_PASSWORD, SOME_PASSWORD_MARKER);

		try (final MarkerBasedDetection detection = new MarkerBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
			switchSessionContextMockTo(sessionContext, "S_2", SOME_USER_ID, SOME_PASSWORD, null);
		}

		verify(sessionContext, times(1)).setSessionAttribute(STALE_SESSION_MARKER, SOME_PASSWORD_MARKER);
	}

	@Test
	public void shouldSetMarkerForNotAnonymousSessionAfterUserChangedToOtherNotAnonymousUser()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", SOME_USER_ID, SOME_PASSWORD, SOME_PASSWORD_MARKER);

		try (final MarkerBasedDetection detection = new MarkerBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
			switchSessionContextMockTo(sessionContext, "S_1", ANOTHER_USER_ID, ANOTHER_PASSWORD, null);
		}

		verify(sessionContext, times(1)).setSessionAttribute(STALE_SESSION_MARKER, ANOTHER_PASSWORD_MARKER);
	}

	@Test
	public void shouldTreatExistingSessionAsNotStaleAndRestoreMarker()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", SOME_USER_ID, SOME_PASSWORD, null);

		try (final MarkerBasedDetection detection = new MarkerBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
		}

		verify(sessionContext, times(1)).setSessionAttribute(STALE_SESSION_MARKER, SOME_PASSWORD_MARKER);
	}

	@Test
	public void shouldDetectStaleSessionAndRestoreMarkerWhenMarkerDoesNotMatch()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", SOME_USER_ID, SOME_PASSWORD, ANOTHER_PASSWORD_MARKER);

		try (final MarkerBasedDetection detection = new MarkerBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isTrue();
		}

		verify(sessionContext, times(1)).setSessionAttribute(STALE_SESSION_MARKER, SOME_PASSWORD_MARKER);
	}

	@Test
	public void shouldResetMarkerWhenPasswordChangedForTheSameUser()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", SOME_USER_ID, SOME_PASSWORD, SOME_PASSWORD_MARKER);

		try (final MarkerBasedDetection detection = new MarkerBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
			detection.passwordChanged(new PasswordChangeEvent(SOME_USER_ID));
			switchSessionContextMockTo(sessionContext, "S_1", SOME_USER_ID, ANOTHER_PASSWORD, SOME_PASSWORD);
		}

		verify(sessionContext, times(1)).setSessionAttribute(STALE_SESSION_MARKER, ANOTHER_PASSWORD_MARKER);
	}

	@Test
	public void shouldNotResetMarkerWhenPasswordChangedForDifferentUser()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", SOME_USER_ID, SOME_PASSWORD, SOME_PASSWORD_MARKER);

		try (final MarkerBasedDetection detection = new MarkerBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
			detection.passwordChanged(new PasswordChangeEvent(ANOTHER_USER_ID));
		}

		verify(sessionContext, never()).setSessionAttribute(any(), any());
	}

	@Test
	public void shouldSetMarkerForNullPassword()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", SOME_USER_ID, null, null);

		try (final MarkerBasedDetection detection = new MarkerBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
		}

		verify(sessionContext, times(1)).setSessionAttribute(STALE_SESSION_MARKER, NULL_PASSWORD_MARKER);
	}

	@Test
	public void shouldTreatNullPasswordAndExistingMarkerAsStaleSession()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", SOME_USER_ID, null, SOME_PASSWORD_MARKER);

		try (final MarkerBasedDetection detection = new MarkerBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isTrue();
		}

		verify(sessionContext, times(1)).setSessionAttribute(STALE_SESSION_MARKER, NULL_PASSWORD_MARKER);
	}

	private SessionContext mockSessionContext(final String sessionId, final String userId, final String userPassword,
	                                          final String marker)
	{
		final SessionContext sessionContext = mock(SessionContext.class);
		switchSessionContextMockTo(sessionContext, sessionId, userId, userPassword, marker);

		return sessionContext;
	}

	private void switchSessionContextMockTo(
			final SessionContext sessionContext, final String sessionId,
			final String userId, final String userPassword, final String marker)
	{
		when(sessionContext.getSessionId()).thenReturn(sessionId);
		when(sessionContext.getSessionUserId()).thenReturn(userId);
		when(sessionContext.getSessionUserPassword()).thenReturn(userPassword);
		when(sessionContext.getSessionAttribute(STALE_SESSION_MARKER)).thenReturn(marker);
		when(sessionContext.getSessionUserToken()).thenReturn(TOKEN);
	}
}