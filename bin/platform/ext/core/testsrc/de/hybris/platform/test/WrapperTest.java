/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test;

import static org.junit.Assert.fail;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.WrapperFactory;

import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.junit.Test;


/**
 * Junit Test for EJBCollections (EJBMap/EJBSet/EJBList)
 */
@IntegrationTest
public class WrapperTest
{

	@Test
	public void testBug1025() throws Exception
	{
		final Set s = new TreeSet();
		s.add("test");
		s.add("string");
		final Set safter = (Set) WrapperFactory.wrap(s);
		if (!(safter instanceof SortedSet))
		{
			fail("SortedSet is not wrapped correctly");
		}


	}

}
