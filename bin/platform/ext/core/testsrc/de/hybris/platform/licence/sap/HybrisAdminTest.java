/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.licence.sap;

import static junit.framework.Assert.fail;
import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.bootstrap.config.ConfigUtil;
import de.hybris.platform.licence.internal.SAPLicenseValidator;
import de.hybris.platform.testframework.HybrisJUnit4Test;
import de.hybris.platform.util.Utilities;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.sap.security.core.server.likey.Persistence;


@IntegrationTest
public class HybrisAdminTest extends HybrisJUnit4Test
{
	private SAPLicenseValidator validator;
	private PropertyBasedTestPersistence persistence;

	@Before
	public void setUp() throws Exception
	{
		persistence = new PropertyBasedTestPersistence();
		validator = new SAPLicenseValidator()
		{
			@Override
			protected Persistence getPersistence()
			{
				return persistence;
			}
		};
		System.setProperty("persistence.impl", PropertyBasedTestPersistence.class.getCanonicalName());
	}

	@After
	public void tearDown() throws Exception
	{
		System.clearProperty("persistence.impl");
		persistence.removePersistenceFile();
	}

	String changeSystemIDTo(final String id)
	{
		return (String) Utilities.loadPlatformProperties().setProperty("license.sap.sapsystem", id);
	}

	void restoreSystemID(final String original)
	{
		if (original == null)
		{
			Utilities.loadPlatformProperties().remove("license.sap.sapsystem");
		}
		else
		{
			Utilities.loadPlatformProperties().setProperty("license.sap.sapsystem", original);
		}
	}


	@Test
	public void shouldInstallTempLicense() throws Exception
	{
		// given
		final String[] args = new String[]
				{ "-t", "CPS_HDB" };
		assertThat(validator.validateLicense("CPS_HDB").isValid()).isFalse();

		// when
		HybrisAdmin.main(args);

		// then
		assertThat(validator.validateLicense("CPS_HDB").isValid()).isTrue();
	}

	@Test
	public void shouldInstallLicenseFromFile() throws Exception
	{
		// given
		final String licenseFileLocation = getLicenseFileLocation();
		writeLicenseFile(licenseFileLocation, getStandardLicenceFileContent());
		final String[] args = new String[]
				{ "-i", licenseFileLocation };
		assertThat(validator.validateLicense("CPS_HDB").isValid()).isFalse();

		// when
		HybrisAdmin.main(args);

		// then
		assertThat(validator.validateLicense("CPS_HDB").isValid()).isTrue();
		FileUtils.deleteQuietly(new File(licenseFileLocation));
	}

	@Test
	public void shouldInstallNonCPSSystemIDLicenseFromFile() throws Exception
	{
		final String defaultSystemID = changeSystemIDTo("CCC"); // change system ID
		try
		{
			final SAPLicenseValidator validatorDifferentSystemID = new SAPLicenseValidator()
			{
				@Override
				protected Persistence getPersistence()
				{
					return persistence;
				}
			};

			// given
			final String licenseFileLocation = getLicenseFileLocation();
			writeLicenseFile(licenseFileLocation, getDifferentSystemIDLicenseFileContent());
			final String[] args = new String[]
					{ "-i", licenseFileLocation };
			assertThat(validatorDifferentSystemID.validateLicense("CPS_MSS").isValid()).isFalse();

			// when
			HybrisAdmin.main(args);

			// then
			assertThat(validatorDifferentSystemID.validateLicense("CPS_MSS").isValid()).isTrue();
			FileUtils.deleteQuietly(new File(licenseFileLocation));
		}
		finally
		{
			restoreSystemID(defaultSystemID);
		}

	}


	@Test
	public void shouldDeleteExistingLicense() throws Exception
	{
		// given
		HybrisAdmin.main(new String[]
				{ "-t", "CPS_HDB" });
		final String[] deleteArgs = new String[]
				{ "-d", "CPS", "Y4989890650", "CPS_HDB" };

		// when
		HybrisAdmin.main(deleteArgs);

		// then
		assertThat(validator.validateLicense("CPS_HDB").isValid()).isFalse();
	}

	private String getLicenseFileLocation()
	{
		return ConfigUtil.getPlatformConfig(HybrisAdminTest.class).getSystemConfig().getTempDir() + "/testLicense.txt";
	}

	private void writeLicenseFile(final String location, final String content)
	{
		final File file = new File(location);
		try
		{
			FileUtils.writeStringToFile(file, content);
		}
		catch (final IOException e)
		{
			fail(e.getMessage());
		}
	}

	String getStandardLicenceFileContent()
	{
		/*
			If license expired:

			https://cxwiki.sap.com/pages/viewpage.action?spaceKey=pskb&title=Retrieving+Internal+SAP+Hybris+Commerce+License
			https://launchpad.support.sap.com/#/licensekey/wizard/installation/SAP-INTERN

			Add a new System:
				Product:  SAP Commerce
				Version:  Use the latest
				System ID: CPS
				System Name: SAP Commerce
				System Type: Development system
				DB: SAP HANA Database
				OS: Linux
				Infrastructure: Private - On Premise

			Add a License Key Request:
				License Type: SAP hybris Commerce Suite
				Hardware Key: Y4989890650
				Valid until:  Defaults to 18 months (maximum available)
		 */
		return "----- Begin SAP License -----\n"
				+ "SAPSYSTEM=CPS\n"
				+ "HARDWARE-KEY=Y4989890650\n"
				+ "INSTNO=SAP-INTERN\n"
				+ "BEGIN=20210221\n"
				+ "EXPIRATION=20220822\n"
				+ "LKEY=MIIBOwYJKoZIhvcNAQcCoIIBLDCCASgCAQExCzAJBgUrDgMCGgUAMAsGCSqGSIb3DQEHATGCAQcwggEDAgEBMFgwUjELMAkGA1UEBhMCREUxHDAaBgNVBAoTE215U0FQLmNvbSBXb3JrcGxhY2UxJTAjBgNVBAMTHG15U0FQLmNvbSBXb3JrcGxhY2UgQ0EgKGRzYSkCAgGhMAkGBSsOAwIaBQCgXTAYBgkqhkiG9w0BCQMxCwYJKoZIhvcNAQcBMBwGCSqGSIb3DQEJBTEPFw0yMTAyMjIxNTEyNTRaMCMGCSqGSIb3DQEJBDEWBBQDkyBOhkPM/5LDtN5iIL1r1L6GMTAJBgcqhkjOOAQDBC8wLQIUI5S+eoFBmdl7aemNez3Pl/Bt5rQCFQCVb78SKq7smmXabDx2ITYYmoxmAA==\n"
				+ "SWPRODUCTNAME=CPS_HDB\n"
				+ "SWPRODUCTLIMIT=2147483647\n"
				+ "SYSTEM-NR=000000000850554657\n";

	}

	String getDifferentSystemIDLicenseFileContent()
	{
		/*
			If license expired:

			https://cxwiki.sap.com/pages/viewpage.action?spaceKey=pskb&title=Retrieving+Internal+SAP+Hybris+Commerce+License
			https://launchpad.support.sap.com/#/licensekey/wizard/installation/SAP-INTERN

			Add a new System:
				Product:  SAP Commerce
				Version:  Use the latest
				System ID: CCC
				System Name: SAP Commerce
				System Type: Development system
				DB: Microsoft SQL Server
				OS: Microsoft Windows
				Infrastructure: Private - On Premise

			Add a License Key Request:
				License Type: SAP hybris Commerce Suite
				Hardware Key: Y4989890650
				Valid until:  Defaults to 18 months (maximum available)
		 */

		return "----- Begin SAP License -----\n"
				+ "SAPSYSTEM=CCC\n"
				+ "HARDWARE-KEY=Y4989890650\n"
				+ "INSTNO=SAP-INTERN\n"
				+ "BEGIN=20210221\n"
				+ "EXPIRATION=20220822\n"
				+ "LKEY=MIIBOgYJKoZIhvcNAQcCoIIBKzCCAScCAQExCzAJBgUrDgMCGgUAMAsGCSqGSIb3DQEHATGCAQYwggECAgEBMFgwUjELMAkGA1UEBhMCREUxHDAaBgNVBAoTE215U0FQLmNvbSBXb3JrcGxhY2UxJTAjBgNVBAMTHG15U0FQLmNvbSBXb3JrcGxhY2UgQ0EgKGRzYSkCAgGhMAkGBSsOAwIaBQCgXTAYBgkqhkiG9w0BCQMxCwYJKoZIhvcNAQcBMBwGCSqGSIb3DQEJBTEPFw0yMTAyMjIxNTI4MTdaMCMGCSqGSIb3DQEJBDEWBBS+XyQ/Pyi3+S4b4dADi6wFgk64BzAJBgcqhkjOOAQDBC4wLAIUTolDoTlFCmez+lSK6v9xzJKROccCFEuSJURVsSTGtd+Inj8OoyY7EnDU\n"
				+ "SWPRODUCTNAME=CPS_MSS\n"
				+ "SWPRODUCTLIMIT=2147483647\n"
				+ "SYSTEM-NR=000000000850554669\n";
	}
}
