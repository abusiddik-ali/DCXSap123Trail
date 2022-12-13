/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.audit;

import de.hybris.bootstrap.annotations.IntegrationTest;

import org.junit.Before;


@IntegrationTest
public class TransactionAwareAuditJaloTest extends TransactionAwareAuditTest
{

	@Before
	public void prepare()
	{
		persistanceLegacyModeSwitcher.switchToValue("true");
		allTypesEnabledSwitcher.switchToValue("true");
		assumeAuditEnabled();
		warmUp();
	}

}
