/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer;

import org.junit.Ignore;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.AbstractJUnit4SpringContextTests;


/**
 * This test marks the ApplicationContext as dirty (see MockTestCleanupListener).
 */
@TestExecutionListeners(
		{ MockTestCleanupListener.class })
public abstract class MockTest extends AbstractJUnit4SpringContextTests
{
	//nothing
}
