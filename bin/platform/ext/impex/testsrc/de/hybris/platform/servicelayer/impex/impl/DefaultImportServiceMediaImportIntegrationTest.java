/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.impex.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.impex.jalo.ImpExManager;
import de.hybris.platform.servicelayer.ServicelayerTest;
import de.hybris.platform.servicelayer.impex.ImportConfig;
import de.hybris.platform.servicelayer.impex.ImportResult;
import de.hybris.platform.servicelayer.media.MediaService;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import javax.annotation.Resource;

import org.junit.Test;


@IntegrationTest
public class DefaultImportServiceMediaImportIntegrationTest extends ServicelayerTest
{
	@Resource
	MediaService mediaService;

	@Test
	public void testLegacyMediaImport()
	{
		// given
		final ImportConfig config = prepareConfig(true, false);

		// when
		final ImportResult result = importService.importData(config);

		// then
		assertThat(result.isError()).isFalse();

		verifyMedia();
	}

	@Test
	public void testLegacyMediaImportWithSldForData()
	{
		// given
		final ImportConfig config = prepareConfig(true, true);

		// when
		final ImportResult result = importService.importData(config);

		// then
		assertThat(result.isError()).isFalse();

		verifyMedia();
	}

	@Test
	public void testNonLegacyMediaImport()
	{
		// given
		final ImportConfig config = prepareConfig(false, false);

		// when
		final ImportResult result = importService.importData(config);

		// then
		assertThat(result.isError()).isFalse();

		verifyMedia();
	}

	@Test
	public void testNonLegacyMediaImportWithSldForData()
	{
		// given
		final ImportConfig config = prepareConfig(false, true);

		// when
		final ImportResult result = importService.importData(config);

		// then
		assertThat(result.isError()).isFalse();

		verifyMedia();
	}

	private ImportConfig prepareConfig(final boolean legacyMode, final boolean sldForData)
	{
		final ImportConfig config = new ImportConfig();

		config.setLegacyMode(legacyMode);
		config.setSldForData(sldForData);

		final ClasspathImpExResource impExResource = new ClasspathImpExResource("/impex/testfiles/import/testmedia.impex",
				StandardCharsets.UTF_8.name());
		config.setScript(impExResource);
		config.setSynchronous(true);
		config.setMaxThreads(2);
		config.setFailOnError(true);

		config.setMediaArchive(new StreamBasedImpExResource(
				ImpExManager.class.getResourceAsStream("/impex/testfiles/import/media/dummymedia/test_3-4.zip"),
				StandardCharsets.UTF_8.name()));

		return config;
	}

	private void verifyMedia()
	{
		final MediaModel media1 = mediaService.getMedia("image_01.jpg");
		final MediaModel media2 = mediaService.getMedia("image_02.jpg");
		final MediaModel media3 = mediaService.getMedia("image_03.jpg");
		final MediaModel media4 = mediaService.getMedia("image_04.jpg");
		final MediaModel media5 = mediaService.getMedia("image_05.jpg");
		final MediaModel media6 = mediaService.getMedia("image_06.jpg");
		final MediaModel media7 = mediaService.getMedia("image_07.jpg");
		final MediaModel media8 = mediaService.getMedia("image_08.jpg");
		final MediaModel media9 = mediaService.getMedia("image_09.jpg");
		final MediaModel media10 = mediaService.getMedia("image_10.jpg");

		assertThat(media1).isNotNull();
		assertThat(media2).isNotNull();
		assertThat(media3).isNotNull();
		assertThat(media4).isNotNull();
		assertThat(media5).isNotNull();
		assertThat(media6).isNotNull();
		assertThat(media7).isNotNull();
		assertThat(media8).isNotNull();
		assertThat(media9).isNotNull();
		assertThat(media10).isNotNull();

		final String mimeType = "image/jpeg";
		assertThat(media1.getMime()).isEqualTo(mimeType);
		assertThat(media2.getMime()).isEqualTo(mimeType);
		assertThat(media3.getMime()).isEqualTo(mimeType);
		assertThat(media4.getMime()).isEqualTo(mimeType);
		assertThat(media5.getMime()).isEqualTo(mimeType);
		assertThat(media6.getMime()).isEqualTo(mimeType);
		assertThat(media7.getMime()).isEqualTo(mimeType);
		assertThat(media8.getMime()).isEqualTo(mimeType);
		assertThat(media9.getMime()).isEqualTo(mimeType);
		assertThat(media10.getMime()).isEqualTo(mimeType);

		assertThat(mediaService.getStreamFromMedia(media1)).hasSameContentAs(getStream("control_img_01.jpg"));
		assertThat(mediaService.getStreamFromMedia(media2)).hasSameContentAs(getStream("control_img_02.jpg"));
		assertThat(mediaService.getStreamFromMedia(media3)).hasSameContentAs(getStream("control_img_03.jpg"));
		assertThat(mediaService.getStreamFromMedia(media4)).hasSameContentAs(getStream("control_img_04.jpg"));
		assertThat(mediaService.getStreamFromMedia(media5)).hasSameContentAs(getStream("control_img_05.jpg"));
		assertThat(mediaService.getStreamFromMedia(media6)).hasSameContentAs(getStream("control_img_06.jpg"));
		assertThat(mediaService.getStreamFromMedia(media7)).hasSameContentAs(getStream("control_img_07.jpg"));
		assertThat(mediaService.getStreamFromMedia(media8)).hasSameContentAs(getStream("control_img_08.jpg"));
		assertThat(mediaService.getStreamFromMedia(media9)).hasSameContentAs(getStream("control_img_09.jpg"));
		assertThat(mediaService.getStreamFromMedia(media10)).hasSameContentAs(getStream("control_img_10.jpg"));
	}

	private static InputStream getStream(final String filename)
	{
		return ImpExManager.class.getResourceAsStream("/impex/testfiles/import/media/dummymedia/check/" + filename);
	}
}
