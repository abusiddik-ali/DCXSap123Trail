/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.jalo.synchronization;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.media.web.TestDefaultMediaFilterLogic;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class SynchronizationDefaultMediaFilterLogicTest extends ServicelayerBaseTest
{
	public static final String MEDIA1 = "thumbnail";
	private static final String CATALOG = "testCatalog";
	private static final String SRC_CATALOG_VERSION = "srcCatalog";
	private static final String DST_CATALOG_VERSION = "dstCatalog";
	private static final String FOLDER_NULL_QUALIFIER = null;
	private String MEDIA_LOCATION1 ;
	@Resource
	ModelService modelService;
	@Resource
	FlexibleSearchService flexibleSearchService;
	@Resource
	MediaService mediaService;

	TestDefaultMediaFilterLogic defaultMediaFilterLogic;

	@Before
	public void setUp()
	{
		defaultMediaFilterLogic = new TestDefaultMediaFilterLogic(flexibleSearchService);
	}

	@Test
	public void testIfNullPersistedContentTypeIsReturnedAsEmptyAfterSync()
	{
		givenTestCatalogWithVersions();

		final MediaModel media1Before = media1From(srcCatalogVersion());
		adjustMediaWithGivenMime(media1Before, null);


		performSynchronization();

		final Optional<PK> dataPK = defaultMediaFilterLogic.extractDataPKFromLocation(MEDIA_LOCATION1);

		final List<String> mimesFromDb = defaultMediaFilterLogic.loadMimesFromDb(dataPK.get());


		final Optional<String> persistedContentType = defaultMediaFilterLogic.getPersistedContentType(FOLDER_NULL_QUALIFIER,
				MEDIA_LOCATION1);

		assertThat(mimesFromDb).containsExactly(null, null);
		assertThat(persistedContentType).isNotPresent().isEqualTo(Optional.empty());
	}

	@Test
	public void testIfInconsistentPersistedContentTypesAreReturnedAsEmptyAfterSync()
	{
		givenTestCatalogWithVersions();

		final MediaModel media1Before = media1From(srcCatalogVersion());
		final String mime1 = "mime1";
		adjustMediaWithGivenMime(media1Before, mime1);

		performSynchronization();

		final MediaModel media1After = media1From(dstCatalogVersion());
		final String mime2 = "mime2";
		adjustMediaWithGivenMime(media1After, mime2);

		final Optional<PK> dataPK = defaultMediaFilterLogic.extractDataPKFromLocation(MEDIA_LOCATION1);
		final List<String> mimesFromDb = defaultMediaFilterLogic.loadMimesFromDb(dataPK.get());

		final Optional<String> persistedContentType = defaultMediaFilterLogic.getPersistedContentType(FOLDER_NULL_QUALIFIER,
				MEDIA_LOCATION1);

		assertThat(mimesFromDb).containsExactlyInAnyOrder(mime1, mime2);
		assertThat(persistedContentType).isNotPresent().isEqualTo(Optional.empty());
	}


	@Test
	public void testIfPersistedContentTypeIsReturnedAfterSync()
	{
		givenTestCatalogWithVersions();

		final MediaModel media1Before = media1From(srcCatalogVersion());
		final String mime = "mime";
		adjustMediaWithGivenMime(media1Before, mime);

		performSynchronization();

		final Optional<PK> dataPK = defaultMediaFilterLogic.extractDataPKFromLocation(MEDIA_LOCATION1);
		final List<String> mimesFromDb =  defaultMediaFilterLogic.loadMimesFromDb(dataPK.get());

		final Optional<String> persistedContentType = defaultMediaFilterLogic.getPersistedContentType(FOLDER_NULL_QUALIFIER,
				MEDIA_LOCATION1);

		assertThat(mimesFromDb).containsExactly(mime, mime);
		assertThat(persistedContentType).isPresent().isEqualTo(Optional.of(mime));
	}

	private void adjustMediaWithGivenMime(final MediaModel mediaModel, final String mime)
	{
		mediaModel.setMime(mime);
		modelService.save(mediaModel);
	}

	private void givenTestCatalogWithVersions()
	{
		final CatalogModel catalog = modelService.create(CatalogModel.class);
		catalog.setId(CATALOG);

		final CatalogVersionModel sourceVersion = modelService.create(CatalogVersionModel.class);
		sourceVersion.setCatalog(catalog);
		sourceVersion.setLanguages(
				Arrays.asList(modelService.get(getOrCreateLanguage("de")), modelService.get(getOrCreateLanguage("en"))));
		sourceVersion.setVersion(SRC_CATALOG_VERSION);

		final CatalogVersionModel targetVersion = modelService.create(CatalogVersionModel.class);
		targetVersion.setCatalog(catalog);
		targetVersion.setLanguages(
				Arrays.asList(modelService.get(getOrCreateLanguage("de")), modelService.get(getOrCreateLanguage("en"))));
		targetVersion.setVersion(DST_CATALOG_VERSION);

		final MediaModel media1 = modelService.create(MediaModel.class);
		media1.setCode(MEDIA1);
		media1.setCatalogVersion(sourceVersion);
		modelService.saveAll();

		MEDIA_LOCATION1 = "testfolderpath/hfd/hcc/"+   media1.getPk() + ".whatever";
		media1.setLocation(MEDIA_LOCATION1);
		media1.setDataPK(media1.getPk().getLong());
		modelService.saveAll();
	}

	private void performSynchronization()
	{
		final SynchronizationTestHelper synchronizationTestHelper = SynchronizationTestHelper
				.builder(srcCatalogVersion(), dstCatalogVersion()).build();
		synchronizationTestHelper.performSynchronization();
	}

	private CatalogVersionModel srcCatalogVersion()
	{
		final CatalogVersionModel example = new CatalogVersionModel();
		example.setCatalog(testCatalog());
		example.setVersion(SRC_CATALOG_VERSION);
		return flexibleSearchService.getModelByExample(example);
	}

	private CatalogModel testCatalog()
	{
		final CatalogModel example = new CatalogModel();
		example.setId(CATALOG);
		return flexibleSearchService.getModelByExample(example);
	}

	private MediaModel media1From(final CatalogVersionModel catalogVersion)
	{
		return mediaService.getMedia(catalogVersion, MEDIA1);
	}

	private CatalogVersionModel dstCatalogVersion()
	{
		final CatalogVersionModel example = new CatalogVersionModel();
		example.setCatalog(testCatalog());
		example.setVersion(DST_CATALOG_VERSION);
		return flexibleSearchService.getModelByExample(example);
	}
}
