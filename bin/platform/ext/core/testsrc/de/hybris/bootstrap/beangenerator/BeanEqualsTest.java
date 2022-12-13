/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.bootstrap.beangenerator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.test.beans.TestBean;

import org.junit.Test;

@UnitTest
public class BeanEqualsTest
{

	@Test
	public void testEquals()
	{
		final TestBean bean = new TestBean();
		bean.setEqualsA("string");
		bean.setEqualsB(Integer.valueOf(1234));
		bean.setEqualsC(Boolean.TRUE);

		final TestBean bean2 = new TestBean();
		bean2.setEqualsA("string");
		bean2.setEqualsB(Integer.valueOf(1234));
		bean2.setEqualsC(Boolean.TRUE);

		assertEquals(bean, bean);
		assertEquals(bean, bean2);

		bean2.setEqualsA("different");
		assertEquals(bean, bean);
		assertNotEquals(bean, bean2);
	}

	@Test
	public void testEqualsCornerCases()
	{
		final TestBean bean = new TestBean();
		// null
		assertNotEquals(bean, null);
		// other type
		assertNotEquals(bean, new Integer(1234));
	}
}
