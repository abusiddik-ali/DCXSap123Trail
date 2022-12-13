/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.classification.features;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.jalo.c2l.Language;
import de.hybris.platform.servicelayer.ServicelayerTest;
import de.hybris.platform.servicelayer.i18n.I18NService;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Lists;


@IntegrationTest
public class LocalizedFeatureWithFallbackIntegrationTest extends ServicelayerTest
{
	private static final String EN_AU = "en-AU";
	private static final String DE_AT = "de-AT";

	@Resource
	protected I18NService i18nService;

	private Map<Locale, List<FeatureValue>> values;
	private LocalizedFeature feature;

	private Language de;
	private Language en;

	private Locale initialLocale;
	private Boolean initialLocalizationFallbackEnabled;

	@Before
	public void setUp()
	{
		de = getOrCreateLanguage("de");
		en = getOrCreateLanguage("en");

		initialLocale = i18nService.getCurrentLocale();
		initialLocalizationFallbackEnabled = i18nService.isLocalizationFallbackEnabled();
		i18nService.setCurrentLocale(de.getLocale());

		values = new HashMap<>();
		values.put(de.getLocale(),
				Lists.newArrayList(new FeatureValue("de-value1", "de-desc1", null), //
						new FeatureValue("de-value2", "de-desc2", null)));
		values.put(en.getLocale(),
				Lists.newArrayList(new FeatureValue("en-value1", "en-desc1", null), //
						new FeatureValue("en-value2", "en-desc2", null)));

		feature = new LocalizedFeature("feature00", values, de.getLocale());
	}

	@After
	public void tearDown()
	{
		i18nService.setCurrentLocale(initialLocale);
		i18nService.setLocalizationFallbackEnabled(initialLocalizationFallbackEnabled);
	}

	@Test
	public void testGetValuesFallbackFromDefaultLanguageFallbackEnabled()
	{
		// given
		final Language otherLanguage = getOrCreateLanguage(DE_AT);
		otherLanguage.setFallbackLanguages(Collections.singletonList(de));
		i18nService.setLocalizationFallbackEnabled(true);

		// when
		final List<FeatureValue> result = feature.getValues(otherLanguage.getLocale());

		// then
		assertThat(result).isNotNull();
		assertThat(result).isNotEmpty();
		assertThat(result).isEqualTo(values.get(de.getLocale()));
	}

	@Test
	public void testGetValueFallbackFromDefaultLanguageFallbackEnabled()
	{
		// given
		final Language otherLanguage = getOrCreateLanguage(DE_AT);
		otherLanguage.setFallbackLanguages(Collections.singletonList(de));
		i18nService.setLocalizationFallbackEnabled(true);

		// when
		final FeatureValue result = feature.getValue(otherLanguage.getLocale());

		// then
		assertThat(result).isNotNull();
		assertThat(result).isEqualTo(values.get(de.getLocale()).get(0));
	}

	@Test
	public void testGetValuesFallbackFromDefaultLanguageFallbackDisabled()
	{
		// given
		final Language otherLanguage = getOrCreateLanguage(DE_AT);
		otherLanguage.setFallbackLanguages(Collections.singletonList(de));
		i18nService.setLocalizationFallbackEnabled(false);

		// when
		final List<FeatureValue> result = feature.getValues(otherLanguage.getLocale());

		// then
		assertThat(result).isNotNull();
		assertThat(result).isEmpty();
	}

	@Test
	public void testGetValueFallbackFromDefaultLanguageFallbackDisabled()
	{
		// given
		final Language otherLanguage = getOrCreateLanguage(DE_AT);
		otherLanguage.setFallbackLanguages(Collections.singletonList(de));
		i18nService.setLocalizationFallbackEnabled(false);

		// when
		final FeatureValue result = feature.getValue(otherLanguage.getLocale());

		// then
		assertThat(result).isNull();
	}

	@Test
	public void testGetValuesFallbackFromNoDefaultLanguageFallbackEnabled()
	{
		// given
		final Language otherLanguage = getOrCreateLanguage(EN_AU);
		otherLanguage.setFallbackLanguages(Collections.singletonList(en));
		i18nService.setLocalizationFallbackEnabled(true);

		// when
		final List<FeatureValue> result = feature.getValues(otherLanguage.getLocale());

		// then
		assertThat(result).isNotNull();
		assertThat(result).isNotEmpty();
		assertThat(result).isEqualTo(values.get(en.getLocale()));
	}

	@Test
	public void testGetValueFallbackFromNoDefaultLanguageFallbackEnabled()
	{
		// given
		final Language otherLanguage = getOrCreateLanguage(EN_AU);
		otherLanguage.setFallbackLanguages(Collections.singletonList(en));
		i18nService.setLocalizationFallbackEnabled(true);

		// when
		final FeatureValue result = feature.getValue(otherLanguage.getLocale());

		// then
		assertThat(result).isNotNull();
		assertThat(result).isEqualTo(values.get(en.getLocale()).get(0));
	}

	@Test
	public void testGetValuesWithoutFallback()
	{
		// given
		final Language otherLanguage = getOrCreateLanguage(DE_AT);
		i18nService.setLocalizationFallbackEnabled(true);

		// when
		final List<FeatureValue> result = feature.getValues(otherLanguage.getLocale());

		// then
		assertThat(result).isNotNull();
		assertThat(result).isEmpty();
	}

	@Test
	public void testGetValueWithoutFallback()
	{
		// given
		final Language otherLanguage = getOrCreateLanguage(DE_AT);
		i18nService.setLocalizationFallbackEnabled(true);

		// when
		final FeatureValue result = feature.getValue(otherLanguage.getLocale());

		// then
		assertThat(result).isNull();
	}
}
