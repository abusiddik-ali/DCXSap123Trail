/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.test;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.Tenant;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.encryption.EncryptionUtil;

import java.util.Map;

import org.assertj.core.api.ThrowableAssert;
import org.junit.After;
import org.junit.Test;


@IntegrationTest
public class EncryptionUtilTest extends ServicelayerBaseTest
{
	private static final String KEYFILE_ID = "1";
	private static final String USER_MIGRATION_KEYFILE_NAME = "migration-key.hybris";
	private final PropertyConfigSwitcher migrationKeyConfiguredByUser = new PropertyConfigSwitcher("symmetric.key.file.2");
	private Tenant currentTenant;

	@After
	public void tearDown()
	{
		migrationKeyConfiguredByUser.switchBackToDefault();
		EncryptionUtil.setKeyfiles(null);
		if (currentTenant != null)
		{
			Registry.setCurrentTenant(currentTenant);
		}
	}

	@Test
	public void shouldNotThrowExceptionDuringMigration()
	{
		final Throwable actual = ThrowableAssert
				.catchThrowable(() -> EncryptionUtil.migrate("User", "passwordAnswer", null));
		assertThat(actual).isNull();
	}

	@Test
	public void shouldReturnOnlyCustomKeyConfiguredBySystem()
	{
		//when
		final Map<String, String> encryptionKeys = EncryptionUtil.getConfiguredEncryptionKeys();

		//then
		assertThat(encryptionKeys).hasSize(1);
		assertThat(encryptionKeys.keySet().stream().findFirst().get()).isEqualTo(KEYFILE_ID);
		assertThat(encryptionKeys.values().stream().findFirst().get()).isEqualTo(EncryptionUtil.DEFAULT_KEYFILE_NAME);
	}

	@Test
	public void shouldReturnAllConfiguredKeys()
	{
		//given
		activateMasterTenant();
		migrationKeyConfiguredByUser.switchToValue(USER_MIGRATION_KEYFILE_NAME);

		//when
		final Map<String, String> encryptionKeys = EncryptionUtil.getConfiguredEncryptionKeys();

		//then
		assertThat(encryptionKeys).hasSize(2);
	}

	@Test
	public void shouldReturnTrueWhenMigrationKeyIsConfigured()
	{
		//given
		activateMasterTenant();
		migrationKeyConfiguredByUser.switchToValue(USER_MIGRATION_KEYFILE_NAME);

		//when
		final boolean migrationKeyConfigured = EncryptionUtil.isConfiguredMigrationKey();

		//then
		assertThat(migrationKeyConfigured).isTrue();
	}

	@Test
	public void shouldReturnFalseWhenMigrationKeyIsNotConfigured()
	{
		//when
		final boolean migrationKeyConfigured = EncryptionUtil.isConfiguredMigrationKey();

		//then
		assertThat(migrationKeyConfigured).isFalse();
	}

	private void activateMasterTenant()
	{
		//EncryptionUtil property configuration depends on master tenant.
		currentTenant = Registry.getCurrentTenantNoFallback();
		Registry.activateMasterTenant();
	}
}
