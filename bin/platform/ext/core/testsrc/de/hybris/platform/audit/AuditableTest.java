/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.audit;

import de.hybris.platform.util.Config;

import org.junit.Assume;


/**
 * Every Audit related tests, which requires the auditing to be enabled should use this interface.
 */
public interface AuditableTest
{
	/**
	 * Assumes the audting is enabled globally. If not - AssumptionViolatedException is thrown, which junit interprets as
	 * test to be ignored.
	 */
	default void assumeAuditEnabled()
	{
		Assume.assumeTrue("Auditing is disabled globally - test will be ignored", Config.getBoolean("auditing.enabled", false));
	}
}
