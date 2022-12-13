/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.licence.sap;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;

import java.util.Vector;

import org.junit.Before;
import org.junit.Test;

import com.sap.security.core.server.likey.Admin;
import com.sap.security.core.server.likey.KeySystem;
import com.sap.security.core.server.likey.LicenseChecker;
import com.sap.security.core.server.likey.LogAndTrace;
import com.sap.security.core.server.likey.Persistence;


@IntegrationTest
public class LicenseCheckerIntegrationTest
{
	private final KeySystem keySystem = new DefaultKeySystem();
	private final LogAndTrace logAndTrace = new DefaultLogAndTrace();
	private Persistence persistence;
	private LicenseChecker licenseChecker;

	@Before
	public void setUp() throws Exception
	{
		persistence = new TestPersistence();
		licenseChecker = new LicenseChecker(persistence, keySystem, logAndTrace);
	}

	@Test
	public void shouldSuccessfullyCheckAndValidateFirstInstalledTempLicense() throws Exception
	{
		// given
		final String swProductName = "CPS_SQL";
		installFirstTempLicenseForProduct(swProductName);

		// when
		final boolean result = licenseChecker.check(swProductName);

		// then
		assertThat(result).isTrue();
	}

	@Test
	public void shouldCheckLicenseWithInvalidProductName() throws Exception
	{
		final String swProductName = "CPS_SQL";
		final String wrongProductName = "WrongProductName";
		installFirstTempLicenseForProduct(swProductName);

		// when
		final boolean result = licenseChecker.check(wrongProductName);

		// then
		assertThat(result).isFalse();
	}

	private void installFirstTempLicenseForProduct(final String productName)
	{
		final Admin admin = new Admin(persistence, keySystem, logAndTrace);
		final boolean installResult = admin.installFirstTempLicense(productName, new Vector());

		assertThat(installResult).isTrue();
	}

}
