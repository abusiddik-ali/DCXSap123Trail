/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core.threadregistry;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.suspend.SuspendResult;
import de.hybris.platform.core.suspend.SystemStatus;

import org.junit.Test;


@UnitTest
public class DefaultSuspendResultUnitTest
{

	@Test
	public void shouldReturnNullResumeTokenWhenSystemIsSuspended()
	{
		final SuspendResult result = DefaultSuspendResult.systemIsSuspendedOrWaiting(SystemStatus.SUSPENDED);

		assertThat(result).isNotNull();
		assertThat(result.getCurrentStatus()).isSameAs(SystemStatus.SUSPENDED);
		assertThat(result.getResumeToken()).isNull();
	}

	@Test
	public void shouldReturnNullResumeTokenWhenSystemIsWaiting()
	{
		final SuspendResult result = DefaultSuspendResult.systemIsSuspendedOrWaiting(SystemStatus.WAITING_FOR_SUSPEND);

		assertThat(result).isNotNull();
		assertThat(result.getCurrentStatus()).isSameAs(SystemStatus.WAITING_FOR_SUSPEND);
		assertThat(result.getResumeToken()).isNull();
	}

	@Test
	public void shouldReturnNullResumeTokenWhenSystemIsWaitingForUpdate()
	{
		final SuspendResult result = DefaultSuspendResult.systemIsSuspendedOrWaiting(SystemStatus.WAITING_FOR_UPDATE);

		assertThat(result).isNotNull();
		assertThat(result.getCurrentStatus()).isSameAs(SystemStatus.WAITING_FOR_UPDATE);
		assertThat(result.getResumeToken()).isNull();
	}

	@Test
	public void shouldThrowIllegalArgumentExceptionWhenSystemIsRunningAndTokenIsNotGiven()
	{
		assertThatExceptionOfType(IllegalArgumentException.class)
				.isThrownBy(() -> DefaultSuspendResult.systemIsSuspendedOrWaiting(SystemStatus.RUNNING));
	}

	@Test
	public void shouldReturnResumeTokenWhenSystemHasBeenSuspended()
	{
		final SuspendResult result = DefaultSuspendResult.systemHasBeenRequestedToSuspend(SystemStatus.RUNNING, "TEST");

		assertThat(result).isNotNull();
		assertThat(result.getCurrentStatus()).isSameAs(SystemStatus.RUNNING);
		assertThat(result.getResumeToken()).isEqualTo("TEST");
	}

}
