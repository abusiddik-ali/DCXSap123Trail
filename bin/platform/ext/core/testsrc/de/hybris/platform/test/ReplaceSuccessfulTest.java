/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test;

import de.hybris.bootstrap.annotations.IntegrationTest;

import org.junit.Test;

import static org.junit.Assert.assertTrue;

/**
 * This test together with {@link ReplaceFailingTest} shows how to replace one test with another.
 */
@IntegrationTest(replaces = ReplaceFailingTest.class)
public class ReplaceSuccessfulTest extends ReplaceFailingTest
{
	@Test
	@Override
	public void failingTest()
	{
		assertTrue("this should NOT fail", true);
	}
}
