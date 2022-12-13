/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.security.permissions;

import de.hybris.platform.servicelayer.security.permissions.impl.DefaultPermissionManagementServiceTest;
import de.hybris.platform.servicelayer.security.strategies.DefaultPermissionCheckValueMappingStrategyTest;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;


@RunWith(Suite.class)
@SuiteClasses(
		{
//
				DefaultPermissionManagementServiceTest.class, //
				PermissionCheckingServiceTest.class, //
				DefaultPermissionCheckValueMappingStrategyTest.class, //
				PermissionCRUDServiceTest.class //
		})
public class PermissionsTestSuite
{
	//
}
