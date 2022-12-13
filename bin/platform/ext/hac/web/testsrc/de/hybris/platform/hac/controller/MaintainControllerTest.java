/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.hac.controller;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.Tenant;
import de.hybris.platform.hac.facade.HacMaintenanceFacade;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.encryption.EncryptionUtil;

import java.util.HashMap;
import java.util.Map;

import javax.annotation.Resource;

import org.assertj.core.api.ThrowableAssert;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class MaintainControllerTest extends ServicelayerBaseTest
{
	private static final String USER_MIGRATION_KEYFILE_NAME = "migration-key.hybris";
	private static final String ACTION_MAP_KEY = "action";
	private static final String SHOW_INFO_ACTION = "showinfo";
	private static final String SHOW_KEYS_ACTION = "showkeys";
	@Resource
	private HacMaintenanceFacade hacMaintenanceFacade;
	private MaintainController maintainController;
	private Tenant currentTenant;
	private final PropertyConfigSwitcher migrationKeyConfiguredByUser = new PropertyConfigSwitcher("symmetric.key.file.2");

	@Before
	public void setUp()
	{
		maintainController = new MaintainController(hacMaintenanceFacade);
	}

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
	public void shouldNotThrowExceptionForMigration()
	{
		final String keysToMigrate = "User.encodedPassword|User.passwordAnswer|User.passwordQuestion";
		final Throwable actual = ThrowableAssert
				.catchThrowable(() -> maintainController.keyMigrationMigrate(keysToMigrate));
		assertThat(actual).isNull();
	}

	@Test
	public void shouldNotThrowExceptionForEmptyMigrationKey()
	{
		final Throwable actual = ThrowableAssert
				.catchThrowable(() -> maintainController.keyMigrationMigrate(""));
		assertThat(actual).isNull();
	}

	@Test
	public void shouldShowInfoWhenMigrationKeyIsNotConfigured()
	{
		//given
		final TestMaintainController testMaintainController = getTestMaintainController();

		//when
		final String action = testMaintainController.keyMigrationData().get(ACTION_MAP_KEY).toString();

		//then
		assertThat(action).isEqualTo(SHOW_INFO_ACTION);
	}

	@Test
	public void shouldShowKeysWhenMigrationKeyIsConfigured()
	{
		//given
		final TestMaintainController testMaintainController = getTestMaintainController();
		activateMasterTenant();
		migrationKeyConfiguredByUser.switchToValue(USER_MIGRATION_KEYFILE_NAME);

		//when
		final String action = testMaintainController.keyMigrationData().get(ACTION_MAP_KEY).toString();

		//then
		assertThat(action).isEqualTo(SHOW_KEYS_ACTION);
	}

	private TestMaintainController getTestMaintainController()
	{
		return new TestMaintainController(hacMaintenanceFacade);
	}

	private void activateMasterTenant()
	{
		//EncryptionUtil property configuration depends on master tenant.
		currentTenant = Registry.getCurrentTenantNoFallback();
		Registry.activateMasterTenant();
	}

	private class TestMaintainController extends MaintainController
	{

		public TestMaintainController(final HacMaintenanceFacade maintenanceFacade)
		{
			super(maintenanceFacade);
		}

		@Override
		public Map<String, Object> keyMigrationData()
		{
			final Map<String, Object> result = new HashMap<>();

			if (!EncryptionUtil.isConfiguredMigrationKey())
			{
				result.put("action", "showinfo");
			}
			else
			{
				result.put("action", "showkeys");
			}

			return result;
		}
	}
}
