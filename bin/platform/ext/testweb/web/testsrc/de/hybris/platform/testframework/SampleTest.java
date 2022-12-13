/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.testframework;

import java.io.NotSerializableException;

import org.junit.Assume;
import org.junit.Ignore;
import org.junit.Test;

public class SampleTest
{

	@Test
	public void testTwoIgnored()
	{
		//
	}

	@Test
	public void testOne() throws InterruptedException
	{
		//
	}

	@Test
	public void testFourAssumeFails()
	{
		Assume.assumeTrue(false);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testFiveThrowExpectedException()
	{
		throw new UnsupportedOperationException();
	}
}