/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util.localization.jalo;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.core.model.c2l.LanguageModel;
import de.hybris.platform.jalo.c2l.Language;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.testframework.seed.TestDataCreator;

import java.util.Properties;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;

public class JaloBasedTypeLocalizationTest extends ServicelayerBaseTest
{

	@Resource
	private ModelService modelService;
	private LanguageModel spanishColombianLang, spanishLang;

	@Before
	public void setUp() throws Exception
	{
		final TestDataCreator creator = new TestDataCreator(modelService);
		spanishColombianLang = creator.createLanguage("es_CO", "Colombian Spanish");
		spanishLang = creator.createLanguage("es", "Spanish");
	}

	@Test
	public void shouldLoadSpanishLocalizationForCoreExtensionProperly()
	{
		// given
		final Language lang = modelService.getSource(spanishLang);

		// when
		final Properties properties = JaloBasedTypeLocalization.loadLocalizations(lang);

		// then
		assertThat(properties).isNotNull();
		assertThat(properties.getProperty("type.retentionstate.processed.name")).isEqualTo("Procesos");
	}

	@Test
	public void shouldLoadColombianSpanishLocalizationForCoreExtensionProperly()
	{
		// given
		final Language lang = modelService.getSource(spanishColombianLang);

		// when
		final Properties properties = JaloBasedTypeLocalization.loadLocalizations(lang);

		// then
		assertThat(properties).isNotNull();
		assertThat(properties.getProperty("type.retentionstate.processed.name")).isEqualTo("Procesado");
	}
}
