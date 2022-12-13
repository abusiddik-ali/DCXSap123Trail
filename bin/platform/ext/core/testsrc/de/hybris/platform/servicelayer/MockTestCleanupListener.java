/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer;

import org.springframework.test.context.TestContext;
import org.springframework.test.context.support.AbstractTestExecutionListener;


public class MockTestCleanupListener extends AbstractTestExecutionListener
{
	@Override
	public void afterTestMethod(final TestContext testContext) throws Exception
	{
		testContext.markApplicationContextDirty(null);

	}
}
