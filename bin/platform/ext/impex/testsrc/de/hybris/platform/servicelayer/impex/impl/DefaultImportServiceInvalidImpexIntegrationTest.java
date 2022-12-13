/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.impex.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.servicelayer.ServicelayerTest;
import de.hybris.platform.servicelayer.impex.ImportConfig;
import de.hybris.platform.servicelayer.impex.ImportResult;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;

import java.util.List;
import java.util.Locale;

import javax.annotation.Resource;

import org.junit.Test;

@IntegrationTest
public class DefaultImportServiceInvalidImpexIntegrationTest extends ServicelayerTest
{
	@Resource
	private ModelService modelService;
	@Resource
	private UserService userService;

	@Test
	public void shouldFailWithInvalidImpexAndSingleThread()
	{
		runTheTest(1);
	}


	@Test
	public void shouldFailWithInvalidImpexAndMultipleThreads()
	{
		runTheTest(2);
	}


	private void runTheTest(final int maxThreads)
	{
		getOrCreateLanguage("de");
		getOrCreateLanguage("en");

		final ImportConfig config = new ImportConfig();
		config.setScript(
				"INSERT_UPDATE Title; code[unique=true]; name[lang=de]; name[lang=en]\n"
						+ ";TTT1;TTT1-de;TTT1-en;\n"
						+ ";TTT2;\"T\"TT2-de\";TTT2-en;\n"
						+ ";TTT3;TTT3-de;TTT3-en;\n"
						+ ";TTT4;TTT4-de;TTT4-en;\n"
						+ ";TTT5;TTT5-de;TTT5-en;\n"
		);

		config.setMaxThreads(maxThreads);
		config.setSynchronous(true);

		final ImportResult importResult = importService.importData(config);

		assertThat(importResult.isFinished()).isTrue();
		assertThat(importResult.isSuccessful()).isFalse();

		for (final String code : List.of("TTT1", "TTT3", "TTT4", "TTT5"))
		{
			assertTitleImported(code, code + "-de", code + "-en");
		}
	}

	void assertTitleImported(final String code, final String nameDE, final String nameEN)
	{
		final TitleModel title = userService.getTitleForCode(code);

		assertThat(title).isNotNull();
		assertThat(modelService.isNew(title)).isFalse();
		assertThat(modelService.isUpToDate(title)).isTrue();

		assertThat(title).extracting(TitleModel::getCode, t -> t.getName(Locale.GERMAN), t -> t.getName(Locale.ENGLISH))
		                 .containsExactly(code, nameDE, nameEN);

	}

}
