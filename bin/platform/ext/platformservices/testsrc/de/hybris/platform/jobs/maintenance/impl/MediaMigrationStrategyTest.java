/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jobs.maintenance.impl;

import static de.hybris.platform.media.storage.impl.LocalFileMediaStorageStrategy.FOLDER_INTEGRITY_VERIFICATION_ENABLED_FLAG;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.jalo.SessionContext;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import org.junit.Test;

@IntegrationTest
public class MediaMigrationStrategyTest extends ServicelayerBaseTest
{
	@Test
	public void shouldExecuteTheSupplierWithVerificationDisabledAndRestorePreviousState()
	{
		setFlagValue(true);
		MediaMigrationStrategy.executeWithDisabledIntegrityVerification(this::verifyFlagIsSetToFalse);
		assertThat(getFlagValue()).isNotNull().isTrue();

		setFlagValue(false);
		MediaMigrationStrategy.executeWithDisabledIntegrityVerification(this::verifyFlagIsSetToFalse);
		assertThat(getFlagValue()).isNotNull().isFalse();

		setFlagValue(null);
		MediaMigrationStrategy.executeWithDisabledIntegrityVerification(this::verifyFlagIsSetToFalse);
		assertThat(getFlagValue()).isNull();
	}

	private Void verifyFlagIsSetToFalse()
	{
		assertThat(getFlagValue()).isNotNull().isFalse();
		return null;
	}

	private Boolean getFlagValue()
	{
		return JaloSession.getCurrentSession().getSessionContext().getAttribute(FOLDER_INTEGRITY_VERIFICATION_ENABLED_FLAG);
	}

	private void setFlagValue(final Boolean value)
	{
		final SessionContext ctx = JaloSession.getCurrentSession().getSessionContext();
		if (value == null)
		{
			ctx.removeAttribute(FOLDER_INTEGRITY_VERIFICATION_ENABLED_FLAG);
		}
		else
		{
			ctx.setAttribute(FOLDER_INTEGRITY_VERIFICATION_ENABLED_FLAG, value);
		}
	}
}