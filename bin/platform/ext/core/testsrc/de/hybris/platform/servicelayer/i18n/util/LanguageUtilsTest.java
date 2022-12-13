/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.servicelayer.i18n.util;

import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import java.util.Arrays;

import org.assertj.core.api.Assertions;
import org.junit.Test;

public class LanguageUtilsTest extends ServicelayerBaseTest
{
	@Test
	public void whenNoLanguagePresent()
	{
		Assertions.assertThat(LanguageUtils.isLanguagePresent("xx")).isFalse();
	}

	@Test
	public void shouldSearchOnlyExactCode()
	{
		getOrCreateLanguage("CAPS");
		getOrCreateLanguage("de");
		Assertions.assertThat(LanguageUtils.isLanguagePresent("CAP")).isFalse();
		Assertions.assertThat(LanguageUtils.isLanguagePresent("caps")).isFalse();
		Assertions.assertThat(LanguageUtils.isLanguagePresent("C%")).isFalse();
		Assertions.assertThat(LanguageUtils.isLanguagePresent("DE")).isFalse();
		Assertions.assertThat(LanguageUtils.isLanguagePresent("d")).isFalse();
	}

	@Test
	public void whenInvalidInput()
	{
		Assertions.assertThat(LanguageUtils.isLanguagePresent("")).isFalse();
		Assertions.assertThat(LanguageUtils.isLanguagePresent(null)).isFalse();
		Assertions.assertThat(LanguageUtils.isLanguagePresent("';\"")).isFalse();
		Assertions.assertThat(LanguageUtils.isLanguagePresent("\0")).isFalse();
		final char[] longtext = new char[65537];
		Arrays.fill(longtext, 'a');
		Assertions.assertThat(LanguageUtils.isLanguagePresent(String.valueOf(longtext))).isFalse();
	}

	@Test
	public void when1LanguageIsPresent()
	{
		getOrCreateLanguage("yy");
		Assertions.assertThat(LanguageUtils.isLanguagePresent("en")).isTrue();
		Assertions.assertThat(LanguageUtils.isLanguagePresent("yy")).isTrue();
	}

	/* having 2 same languages is impossible because not only of UniqueAttributesInterceptor
	 * but also de.hybris.platform.jalo.c2l.C2LItem.checkConsistencyIsocode()
	 * also, having 2 same languages would probably cause system to explode in many other places
	 * not testing this case */
}