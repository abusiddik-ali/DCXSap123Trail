/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.bootstrap.beangenerator;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.bootstrap.beangenerator.definitions.model.ClassNamePrototype;

import java.util.Arrays;
import java.util.Collections;

import junit.framework.Assert;

import org.junit.Test;


@UnitTest
public class ClassNameUtilTest
{
	@Test
	public void testPrototypeNone()
	{

		final ClassNamePrototype proto = ClassNameUtil.toPrototype("java.lang.String");

		Assert.assertEquals("java.lang.String", proto.getBaseClass());
		Assert.assertEquals(Collections.EMPTY_LIST, proto.getPrototypes());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testPrototypeInvalid1()
	{
		ClassNameUtil.toPrototype("de.hybris.simple.Simple<");
	}

	@Test(expected = IllegalArgumentException.class)
	public void testPrototypeInvalid2()
	{
		ClassNameUtil.toPrototype("de.hybris.simple.Simple<Pojo");
	}

	@Test(expected = IllegalArgumentException.class)
	public void testPrototypeInvalid3()
	{
		ClassNameUtil.toPrototype("de.hybris.simple.Simple<Pojo<");
	}

	@Test(expected = IllegalArgumentException.class)
	public void testPrototypeInvalid4()
	{
		ClassNameUtil.toPrototype("de.hybris.simple.Simple>Pojo");
	}

	@Test
	public void testPrototypeSimple()
	{

		final ClassNamePrototype proto = ClassNameUtil.toPrototype("de.hybris.simple.Simple<Pojo>");

		Assert.assertEquals("de.hybris.simple.Simple", proto.getBaseClass());
		Assert.assertEquals(Arrays.asList(new ClassNamePrototype("Pojo")), proto.getPrototypes());
	}

	@Test
	public void testPrototypeSimpleWithExtendsGeneric()
	{

		final ClassNamePrototype proto = ClassNameUtil
				.toPrototype("de.hybris.test.data.Common<T extends de.hybris.test.data.beans.holders.HolderData>");

		Assert.assertEquals("de.hybris.test.data.Common", proto.getBaseClass());
		Assert.assertEquals(Arrays.asList(new ClassNamePrototype("T extends de.hybris.test.data.beans.holders.HolderData")),
				proto.getPrototypes());
	}


	@Test
	public void testPrototypeMulti()
	{

		final ClassNamePrototype proto = ClassNameUtil.toPrototype("de.hybris.simple.Simple<Pojo,Dojo>");

		Assert.assertEquals("de.hybris.simple.Simple", proto.getBaseClass());
		Assert.assertEquals(Arrays.asList(new ClassNamePrototype("Pojo"), new ClassNamePrototype("Dojo")), proto.getPrototypes());
	}


	@Test
	public void testPrototypeNested()
	{

		final ClassNamePrototype proto = ClassNameUtil.toPrototype("de.hybris.simple.Simple<Pojo,Dojo<Mojo,Jojo>>");

		Assert.assertEquals("de.hybris.simple.Simple", proto.getBaseClass());
		Assert.assertEquals(Arrays.asList(new ClassNamePrototype("Pojo"), new ClassNamePrototype("Dojo", new ClassNamePrototype(
				"Mojo"), new ClassNamePrototype("Jojo"))), proto.getPrototypes());
	}


	@Test
	public void testPrototypeWicked()
	{

		final ClassNamePrototype proto = ClassNameUtil.toPrototype("de.hybris.simple.Simple<A<B,C>,D<E<F>,G<H,I>>>");

		Assert.assertEquals("de.hybris.simple.Simple", proto.getBaseClass());

		Assert.assertEquals(Arrays.asList(new ClassNamePrototype("A",//
						new ClassNamePrototype("B"), new ClassNamePrototype("C"))//
				, new ClassNamePrototype("D", new ClassNamePrototype("E", new ClassNamePrototype("F")),//
						new ClassNamePrototype("G", new ClassNamePrototype("H"), new ClassNamePrototype("I")))),
				proto.getPrototypes());//
	}


	@Test
	public void testShortenedClassnameWithExtendsGeneric()
	{

		final ClassNamePrototype proto = ClassNameUtil
				.toPrototype("de.hybris.test.data.Common<T extends de.hybris.test.data.beans.holders.HolderData>");
		final String given = ClassNameUtil.getShortClassName(proto);

		Assert.assertEquals("Common<T extends de.hybris.test.data.beans.holders.HolderData>", given);

	}


	@Test
	public void testShortenedClassname()
	{

		final ClassNamePrototype proto = ClassNameUtil
				.toPrototype("de.hybris.simple.Simple<some.bla.Pojo,other.blah.Dojo<yep.here.also.Mojo,you.wont.see.me.Jojo>>");

		final String given = ClassNameUtil.getShortClassName(proto);

		Assert.assertEquals("Simple<Pojo,Dojo<Mojo,Jojo>>", given);

	}

}
