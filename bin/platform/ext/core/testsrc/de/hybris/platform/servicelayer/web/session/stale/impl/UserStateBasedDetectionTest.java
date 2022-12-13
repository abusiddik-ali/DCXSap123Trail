/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.web.session.stale.impl;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.servicelayer.user.listener.PasswordChangeEvent;
import org.joda.time.Instant;
import org.junit.Test;

import java.util.Date;

import static de.hybris.platform.core.Constants.USER.ADMIN_EMPLOYEE;
import static de.hybris.platform.core.Constants.USER.ANONYMOUS_CUSTOMER;
import static de.hybris.platform.servicelayer.web.session.stale.impl.MarkerBasedDetection.STALE_SESSION_MARKER;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@UnitTest
public class UserStateBasedDetectionTest
{
	private final String ANONYMOUS_USER_ID = ANONYMOUS_CUSTOMER;
	private final String userName = "some.user.name";
	private final Date futureDate = new Instant().plus(10*60*1000l).toDate();
	private final Date pastDate = new Instant().minus(10*60*1000l).toDate();

	@Test
	public void shouldNotInvalidateDisabledAnonymousUserSession()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", ANONYMOUS_USER_ID, true, futureDate);

		try (final UserStateBasedDetection detection = new UserStateBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
		}
	}

	@Test
	public void shouldNotInvalidateEnabledAnonymousUserSession()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", ANONYMOUS_USER_ID, false, futureDate);

		try (final UserStateBasedDetection detection = new UserStateBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
		}
	}

	@Test
	public void shouldNotInvalidateOutdatedDisabledAnonymousUserSession()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", ANONYMOUS_USER_ID, true, pastDate);

		try (final UserStateBasedDetection detection = new UserStateBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
		}
	}

	@Test
	public void shouldNotInvalidateOutdatedEnabledAnonymousUserSession()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", ANONYMOUS_USER_ID, false, pastDate);

		try (final UserStateBasedDetection detection = new UserStateBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
		}
	}

	@Test
	public void shouldNotInvalidateDisabledAdminSession()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", ADMIN_EMPLOYEE, true, futureDate);

		try (final UserStateBasedDetection detection = new UserStateBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
		}
	}

	@Test
	public void shouldNotInvalidateEnabledAdminSession()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", ADMIN_EMPLOYEE, false, futureDate);

		try (final UserStateBasedDetection detection = new UserStateBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
		}
	}

	@Test
	public void shouldNotInvalidateOutdatedDisabledAdminSession()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", ADMIN_EMPLOYEE, true, pastDate);

		try (final UserStateBasedDetection detection = new UserStateBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
		}
	}

	@Test
	public void shouldNotInvalidateOutdatedEnabledAdminSession()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", ADMIN_EMPLOYEE, false, pastDate);

		try (final UserStateBasedDetection detection = new UserStateBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
		}
	}

	@Test
	public void shouldInvalidateDisabledUserSession()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", userName, true, futureDate);

		try (final UserStateBasedDetection detection = new UserStateBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isTrue();
		}
	}

	@Test
	public void shouldNotInvalidateEnabledUserSession()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", userName, false, futureDate);

		try (final UserStateBasedDetection detection = new UserStateBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isFalse();
		}
	}

	@Test
	public void shouldInvalidateOutdatedDisabledUserSession()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", userName, true, pastDate);

		try (final UserStateBasedDetection detection = new UserStateBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isTrue();
		}
	}

	@Test
	public void shouldInvalidateEnabledOutdatedUserSession()
	{
		final SessionContext sessionContext = mockSessionContext("S_1", userName, false, pastDate);

		try (final UserStateBasedDetection detection = new UserStateBasedDetection(sessionContext))
		{
			assertThat(detection.isStaleSession()).isTrue();
		}
	}

	private SessionContext mockSessionContext(final String sessionId, final String userId, final boolean loginDisabled, final Date deactivationDate)
	{
		final SessionContext sessionContext = mock(SessionContext.class);
		switchSessionContextMockTo(sessionContext, sessionId, userId, loginDisabled, deactivationDate);

		return sessionContext;
	}

	private void switchSessionContextMockTo(
			final SessionContext sessionContext, final String sessionId,
			final String userId, final boolean loginDisabled, final Date deactivationDate)
	{
		when(sessionContext.getSessionId()).thenReturn(sessionId);
		when(sessionContext.getSessionUserId()).thenReturn(userId);
		when(sessionContext.isSessionUserLoginDisabled()).thenReturn(loginDisabled);
		when(sessionContext.getSessionUserDeactivationDate()).thenReturn(deactivationDate);
	}
}
