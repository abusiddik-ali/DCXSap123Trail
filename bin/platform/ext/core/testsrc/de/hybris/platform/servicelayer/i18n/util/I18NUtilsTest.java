/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.i18n.util;

import org.junit.Assert;
import org.junit.Test;


public class I18NUtilsTest
{
	@Test
	public void testIsFormerISOCode()
	{
		// "iw"->"he", "ji"->"yi", "in"->"id"
		Assert.assertTrue("code should belong to FORMER_ISO_CODES", I18NUtils.isFormerISOCode("iw"));
		Assert.assertTrue("code should belong to FORMER_ISO_CODES", I18NUtils.isFormerISOCode("ji"));
		Assert.assertTrue("code should belong to FORMER_ISO_CODES", I18NUtils.isFormerISOCode("in"));
	}

	@Test
	public void testMapFormerISOCodeToActual()
	{
		// "iw"->"he", "ji"->"yi", "in"->"id"
		Assert.assertEquals("code should be equal to \"he\"", "he", I18NUtils.mapFormerISOCodeToActual("iw"));
		Assert.assertEquals("code should be equal to \"yi\"", "yi", I18NUtils.mapFormerISOCodeToActual("ji"));
		Assert.assertEquals("code should be equal to \"id\"", "id", I18NUtils.mapFormerISOCodeToActual("in"));
	}
}
