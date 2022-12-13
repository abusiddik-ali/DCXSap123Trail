/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.media.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.catalog.model.CatalogUnawareMediaModel;
import de.hybris.platform.core.model.media.MediaFolderModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.media.url.impl.LocalMediaWebURLStrategy;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.Config;

import java.util.Objects;
import java.util.function.Consumer;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;

public abstract class AbstractMediaStoragePrettyUrlIntegrationTest extends ServicelayerBaseTest
{
	private final PropertyConfigSwitcher prettyUrlEnabledProperty = new PropertyConfigSwitcher(
			LocalMediaWebURLStrategy.MEDIA_LEGACY_PRETTY_URL);
	@Resource
	MediaService mediaService;
	@Resource
	ModelService modelService;

	@Before
	public void createTestMediaFolder()
	{
		final MediaFolderModel mediaFolder = modelService.create(MediaFolderModel.class);
		mediaFolder.setQualifier(getTestFolderQualifier());
		mediaFolder.setPath("testpath");
		modelService.saveAll();
		modelService.detachAll();

	}

	@After
	public void restorePrettyUrlFlag()
	{
		prettyUrlEnabledProperty.switchBackToDefault();
	}

	@Test
	public void shouldGenerateContextBasedUrlsWhenPrettyUrlIsDisabled()
	{
		Assume.assumeTrue(isTestEnabled());
		prettyUrlEnabledProperty.switchToValue(Boolean.FALSE.toString());
		iterateOverMedias(this::requireContextBasedUrl);
	}

	@Test
	public void shouldGeneratePrettyUrlIfPossibleWhenPrettyUrlIsEnabled()
	{
		Assume.assumeTrue(isTestEnabled());
		prettyUrlEnabledProperty.switchToValue(Boolean.TRUE.toString());
		iterateOverMedias(this::requirePrettyUrlIfPossibleBasedUrl);
	}

	protected abstract String getTestFolderQualifier();

	protected boolean isTestEnabled()
	{
		return true;
	}

	protected boolean isStorageStrategyUnderTest(final String storageExtensionName)
	{
		return Objects.requireNonNull(storageExtensionName).equals(Config.getParameter("media.storage.strategy.under.test"));
	}

	protected void requirePrettyUrlIfPossibleBasedUrl(final MediaModel mediaModel)
	{
		if (!isPrettyUrlPossible(mediaModel))
		{
			requireContextBasedUrl(mediaModel);
		}
		else
		{
			assertThat(mediaModel.getURL()).isNotNull().doesNotContain("context=").matches(".+\\..+");
		}
	}

	protected void requireContextBasedUrl(final MediaModel mediaModel)
	{
		assertThat(mediaModel.getURL()).isNotNull().contains("context=");
	}

	protected void iterateOverMedias(final Consumer<MediaModel> mediaConsumer)
	{
		Objects.requireNonNull(mediaConsumer);

		for (final var fileName : new String[]{ null, "test.jpg", "test.jpeg", "test.html", "test.htm", "test", "test.21.txt" })
		{
			for (final var content : new byte[][]{ "".getBytes(), "<html><head>".getBytes(), new byte[]{ 1, 2, 3, 4 }, new byte[]{ (byte) 0xFF, (byte) 0xD8, (byte) 0xFF } })
			{
				modelService.detachAll();
				final MediaFolderModel mediaFolder = mediaService.getFolder(getTestFolderQualifier());
				final MediaModel media = createMedia(mediaFolder, fileName, content);
				mediaConsumer.accept(media);
			}
		}
	}

	protected MediaModel createMedia(final MediaFolderModel mediaFolder, final String fileName, final byte[] content)
	{
		final MediaModel media = modelService.create(CatalogUnawareMediaModel.class);
		media.setFolder(mediaFolder);
		media.setRealFileName(fileName);
		media.setCode("TEST_" + java.util.UUID.randomUUID());
		modelService.saveAll();
		mediaService.setDataForMedia(media, content);
		return media;
	}

	protected boolean isPrettyUrlPossible(final MediaModel mediaModel)
	{
		return true;
	}
}
