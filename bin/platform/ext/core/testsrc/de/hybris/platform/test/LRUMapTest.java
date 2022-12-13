/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test;

import static org.junit.Assert.assertEquals;

import de.hybris.bootstrap.annotations.UnitTest;

import java.util.Map;

import org.apache.commons.collections.map.LRUMap;
import org.junit.Test;


@UnitTest
public class LRUMapTest extends AbstractMapTest
{

	@Override
	protected Map createMapInstance()
	{
		return new LRUMap(2);
	}

	@Test
	public void testBug()
	{
		map.put("1", "eins");
		map.put("2", "zwei");
		assertEquals("eins", map.get("1"));
		assertEquals("zwei", map.get("2"));

		map.put("3", "drei");
		assertEquals(null, map.get("1"));
		assertEquals("zwei", map.get("2"));
		assertEquals("drei", map.get("3"));

		map.get("2");
		map.put("4", "vier");

		assertEquals(null, map.get("1"));
		assertEquals("zwei", map.get("2"));
		assertEquals(null, map.get("3"));
		assertEquals("vier", map.get("4"));
	}

	@Test
	public void testAgain()
	{
		map.put("2", "zwei");
		map.put("3", "drei");

		map.get("2");
		map.put("4", "vier");

		assertEquals("zwei", map.get("2"));
		assertEquals(null, map.get("3"));
		assertEquals("vier", map.get("4"));
	}
}
