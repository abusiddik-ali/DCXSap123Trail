/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.testframework;

import org.apache.log4j.Logger;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;


public class SampleTestCornerCaseBeforeAssume
{
	private static final Logger LOG = Logger.getLogger(SampleTestCornerCaseBeforeAssume.class);

	@Before
	public void prepare()
	{
		Assume.assumeTrue(false);
	}

	@Test
	public void test()
	{
		Assert.assertTrue(true);
	}

}
