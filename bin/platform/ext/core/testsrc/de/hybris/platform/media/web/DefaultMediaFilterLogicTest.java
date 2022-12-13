/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.media.web;

import static de.hybris.platform.media.services.impl.DefaultMimeService.FALLBACK_MIME;

import static org.apache.commons.lang.StringUtils.EMPTY;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.model.media.MediaFolderModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.jalo.media.MediaManager;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.RandomStringUtils;
import org.assertj.core.api.AssertionsForClassTypes;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class DefaultMediaFilterLogicTest extends ServicelayerBaseTest
{
	private static final String FAKE_LOCATION = "/8796716531742/nonexistinglocation.xd";
	private static final String SPOOF_MIME = "spoof/mime";
	private final PropertyConfigSwitcher contentTypeFromDB = new PropertyConfigSwitcher("media.filter.contentType.fromDB");

	@Resource
	private ModelService modelService;

	@Resource
	private FlexibleSearchService flexibleSearchService;

	@Resource
	private MediaService mediaService;

	private TestDefaultMediaFilterLogic mediaFilterLogic;
	private HttpServletResponse httpResponse;

	@Before
	public void setUp()
	{
		mediaFilterLogic = spy(new TestDefaultMediaFilterLogic(flexibleSearchService));
		httpResponse = mock(HttpServletResponse.class);

		contentTypeFromDB.switchToValue(Boolean.TRUE.toString());
	}

	@After
	public void tearDown() throws Exception
	{
		contentTypeFromDB.switchBackToDefault();
	}

	@Test
	public void testIfPersistedContentTypeIsRetrievedFromDb()
	{
		final String persistedMime = "dbMime";
		final String location = createMediaModelWithGivenMimeType(persistedMime).getLocation();
		final Optional<String> persistedContentType = mediaFilterLogic.getPersistedContentType(null, location);
		AssertionsForClassTypes.assertThat(persistedContentType.orElseThrow()).isEqualTo(persistedMime);
	}

	@Test
	public void testIfPersistedContentTypeIsRetrievedFromDbWhenMediaInFolder()
	{
		final String persistedMime = "dbMime";
		final String folderQualifier = "randomQualifier_"+ RandomStringUtils.randomAlphanumeric(5);

		final MediaFolderModel folder = createMediaFolder(folderQualifier);
		final String location = createMediaModelWithGivenMimeType(persistedMime, folder).getLocation();
		final Optional<String> persistedContentType = mediaFilterLogic.getPersistedContentType(folderQualifier, location);
		AssertionsForClassTypes.assertThat(persistedContentType.orElseThrow()).isEqualTo(persistedMime);
	}

	private MediaFolderModel createMediaFolder(final String randomQualifier)
	{
		final MediaFolderModel folder = modelService.create(MediaFolderModel.class);
		folder.setPath("some/random/path");
		folder.setQualifier(randomQualifier);
		modelService.save(folder);
		return folder;
	}

	@Test
	public void testIfDbContentTypeForFakeLocationIsReturnedAsEmpty()
	{
		final Optional<String> persistedContentType = mediaFilterLogic.getPersistedContentType(null, FAKE_LOCATION);
		AssertionsForClassTypes.assertThat(persistedContentType).isEmpty();
	}

	@Test
	public void testIfEmptyPersistedContentTypeIsReturnedAsEmpty()
	{
		final String persistedMime = EMPTY;
		final String location = createMediaModelWithGivenMimeType(persistedMime).getLocation();
		final Optional<String> persistedContentType = mediaFilterLogic.getPersistedContentType(null, location);
		AssertionsForClassTypes.assertThat(persistedContentType).isEmpty();
	}

	@Test
	public void testIfNullPersistedContentTypeIsReturnedAsEmpty()
	{
		final String persistedMime = null;
		final String location = createMediaModelWithGivenMimeType(persistedMime).getLocation();
		final Optional<String> persistedContentType = mediaFilterLogic.getPersistedContentType(null, location);
		AssertionsForClassTypes.assertThat(persistedContentType).isEmpty();
	}

	@Test
	public void testIfDetectContentTypeWorksBasingOnBytes()
	{
		final String mediaStreamRelatedMime = MimeTypeFirstBytes.IMAGE_PNG.getMimeType();
		mockReturnedBytesForGivenMimeType(mediaStreamRelatedMime);
		mediaFilterLogic.addContentType(httpResponse, generateMediaContextWithSpoofMime(8796716531742l), EMPTY);
		verify(httpResponse, times(1)).setContentType(mediaStreamRelatedMime);
	}

	@Test
	public void testIfContentTypeIsTakenFromContextIfTakingFromDBIsDisabled()
	{
		contentTypeFromDB.switchToValue(Boolean.FALSE.toString());

		final String persistedMime = "dbMime";
		final String mediaStreamRelatedMime = MimeTypeFirstBytes.IMAGE_PNG.getMimeType();
		mockReturnedBytesForGivenMimeType(mediaStreamRelatedMime);
		final MediaModel media = createMediaModelWithGivenMimeType(persistedMime);
		mediaFilterLogic.addContentType(httpResponse, generateMediaContextWithSpoofMime(media.getPk().getLongValue()), EMPTY);
		verify(httpResponse, times(1)).setContentType(SPOOF_MIME);
	}

	@Test
	public void testIfDetectContentTypeWorksBasingOnLocation()
	{
		final String mediaLocation = "name.jpg";
		final String mediaStreamRelatedMime = MimeTypeFirstBytes.BLANK.getMimeType();
		mockReturnedBytesForGivenMimeType(mediaStreamRelatedMime);
		mediaFilterLogic.addContentType(httpResponse, generateMediaContextWithSpoofMime(mediaLocation), mediaLocation);
		verify(httpResponse, times(1)).setContentType("image/jpeg");
	}

	@Test
	public void testIfDbRelatedContentTypeTakesPrecedenceOverDetectedOne()
	{
		final String persistedMime = "dbMime";
		final String mediaStreamRelatedMime = MimeTypeFirstBytes.IMAGE_PNG.getMimeType();
		mockReturnedBytesForGivenMimeType(mediaStreamRelatedMime);
		final MediaModel media = createMediaModelWithGivenMimeType(persistedMime);
		mediaFilterLogic.addContentType(httpResponse, generateMediaContextWithSpoofMime(media.getPk().getLongValue()), EMPTY);
		verify(httpResponse, times(1)).setContentType(persistedMime);
		verify(httpResponse, never()).setContentType(SPOOF_MIME);
	}

	@Test
	public void testIfContentTypeIsBasedOnDetectedOneWhenDbMimeIsEmpty()
	{
		final String persistedMime = null;
		final String mediaStreamRelatedMime = MimeTypeFirstBytes.IMAGE_PNG.getMimeType();
		mockReturnedBytesForGivenMimeType(mediaStreamRelatedMime);
		final MediaModel media = createMediaModelWithGivenMimeType(persistedMime);
		mediaFilterLogic.addContentType(httpResponse, generateMediaContextWithSpoofMime(media.getPk().getLongValue()), EMPTY);
		verify(httpResponse, times(1)).setContentType(mediaStreamRelatedMime);
		verify(httpResponse, never()).setContentType(SPOOF_MIME);
	}

	@Test
	public void testIfDefaultContentTypeIsReturnedWhenCantBeEstablishedByDbAndDetection()
	{
		final String persistedMime = null;
		final String mediaStreamRelatedMime = MimeTypeFirstBytes.BLANK.getMimeType();
		mockReturnedBytesForGivenMimeType(mediaStreamRelatedMime);
		final MediaModel media = createMediaModelWithGivenMimeType(persistedMime);
		mediaFilterLogic.addContentType(httpResponse, generateMediaContextWithSpoofMime(media.getPk().getLongValue()), EMPTY);
		verify(httpResponse, times(1)).setContentType(FALLBACK_MIME);
		verify(httpResponse, never()).setContentType(SPOOF_MIME);
	}

	private MediaModel createMediaModelWithGivenMimeType(final String contentType)
	{
		return createMediaModelWithGivenMimeType(contentType, null);
	}

	private MediaModel createMediaModelWithGivenMimeType(final String contentType,
	                                               final MediaFolderModel folder)
	{
		final MediaModel mediaModel = modelService.create(MediaModel.class);
		mediaModel.setMime(contentType);
		mediaModel.setCode("TEST_CODE");
		mediaModel.setCatalogVersion(createCatalogVersionModel());
		if (folder != null)
		{
			mediaModel.setFolder(folder);
		}
		modelService.saveAll();

		mediaModel.setLocation(generateLocation(mediaModel.getPk().getLongValue()));
		mediaModel.setDataPK(mediaModel.getPk().getLong());
		modelService.saveAll();

		return mediaModel;
	}

	private String generateLocation(final long pk)
	{
		return "/" + pk + "/samplelocation.xd";
	}
	
	private CatalogVersionModel createCatalogVersionModel()
	{
		final CatalogModel catalog = modelService.create(CatalogModel.class);
		catalog.setId("TEST_CATALOG");
		final CatalogVersionModel catalogVersionModel = modelService.create(CatalogVersionModel.class);
		catalogVersionModel.setCatalog(catalog);
		catalogVersionModel.setVersion("TEST_VERSION_1");
		return catalogVersionModel;
	}

	private void mockReturnedBytesForGivenMimeType(final String mimeType)
	{
		final MediaManager.InputStreamWithSize stream = new MediaManager.InputStreamWithSize(
				MimeTypeFirstBytes.getMimeTypeRelatedStream(mimeType), MimeTypeFirstBytes.N_BYTES);
		doReturn(stream).when(mediaFilterLogic).getMediaAsStreamWithSize(anyString(), anyString());
	}

	private Iterable<String> generateMediaContextWithSpoofMime(final String location)
	{
		final List<String> mediaContext = new ArrayList<>();
		mediaContext.add("master"); // TENANT
		mediaContext.add("root"); // FOLDER
		mediaContext.add("338594"); // SIZE
		mediaContext.add(SPOOF_MIME); // MIME
		mediaContext.add(location); // LOCATION
		mediaContext.add("1b6147960b8bbb57e38d6ae1735ee8f7883db459b277f3f6d549a54fa1809450"); // LOCATION_HASH
		return mediaContext;
	}

	private Iterable<String> generateMediaContextWithSpoofMime(final long pk)
	{
		return generateMediaContextWithSpoofMime(generateLocation(pk));
	}

	private enum MimeTypeFirstBytes
	{
		IMAGE_PNG(new byte[]
				{
						-119, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 2, -68
				},
				"image/png"),

		BLANK(new byte[]
				{
						0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
				},
				"non-existing/mime");

		final static int N_BYTES = 20;
		final byte[] firstBytes;
		final String mimeType;

		MimeTypeFirstBytes(final byte[] firstBytes, final String mimeType)
		{
			this.firstBytes = firstBytes;
			this.mimeType = mimeType;
		}

		public static ByteArrayInputStream getMimeTypeRelatedStream(final String mimeType)
		{
			return new ByteArrayInputStream(Stream.of(MimeTypeFirstBytes.values())
			                                      .filter(m -> m.mimeType.equals(mimeType))
			                                      .findFirst()
			                                      .map(m -> m.firstBytes)
			                                      .orElse(BLANK.firstBytes));
		}

		public String getMimeType()
		{
			return mimeType;
		}
	}
}
