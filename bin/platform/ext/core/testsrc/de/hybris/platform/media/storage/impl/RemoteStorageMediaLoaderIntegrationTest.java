/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.media.storage.impl;

import static de.hybris.platform.testframework.assertions.assertj.ExtendedAssertions.assertThat;
import static org.assertj.core.api.Assertions.fail;
import static org.mockito.BDDMockito.given;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.media.MediaFolderModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.media.storage.MediaStorageConfigService;
import de.hybris.platform.media.storage.MediaStorageConfigService.MediaFolderConfig;
import de.hybris.platform.media.storage.MediaStorageStrategy;
import de.hybris.platform.media.storage.impl.DefaultLocalMediaFileCacheService.MediaCacheUnit;
import de.hybris.platform.regioncache.CacheController;
import de.hybris.platform.regioncache.key.CacheKey;
import de.hybris.platform.regioncache.region.CacheRegion;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.media.impl.DefaultMediaService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.MediaUtil;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.Nullable;
import javax.annotation.Resource;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.RandomStringUtils;
import org.apache.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.google.common.base.Optional;
import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;


@IntegrationTest
public class RemoteStorageMediaLoaderIntegrationTest extends ServicelayerBaseTest
{
	private static final Logger LOG = Logger.getLogger(RemoteStorageMediaLoaderIntegrationTest.class);

	private static final String DUMMY_MEDIA_STORAGE_STRATEGY_ID = "localFileMediaStorageStrategy";
	private static final String IMAGE_JPEG = "image/jpeg";
	private static final String JPG_EXTENSION = ".jpg";
	private static final String LONG_FILENAME = "Curel Whitening Gift Set Curel Whitening Moisture Care 40g Curel Whitening Moisture Lotion II 140ml Free Curel Trail Set -BP_277662";
	private static final String PATH = "some/long/path/and/some/more/";
	private static final String TENANT_ID = Registry.getCurrentTenantNoFallback().getTenantID();
	private static final String CACHE_FOLDER = "cache";

	private final PropertyConfigSwitcher preventLongfilenamesSwitcher = new PropertyConfigSwitcher(
			"prevent.longfilenames.localcache");
	private final PropertyConfigSwitcher fooBarStorage = new PropertyConfigSwitcher("media.folder.fooBar.storage.strategy");

	@Resource
	private MediaStorageConfigService mediaStorageConfigService;
	@Resource
	private DefaultMediaStorageRegistry defaultMediaStorageRegistry;
	@Resource
	private ModelService modelService;
	@Resource
	private DefaultMediaService mediaService;
	@Resource
	private DefaultLocalMediaFileCacheService localMediaFileCacheService;
	@Resource(name = "defaultCacheController")
	private CacheController cacheController;
	@Resource
	private MediaCacheRecreator localMediFileCacheRecreator;

	private MediaModel media1, media2, media3;
	private MediaFolderModel sampleFolder;

	private CatalogVersionModel catalogVersion;
	private MediaStorageConfigService.MediaFolderConfig sampleFolderConfig;
	private CatalogModel catalog;

	private Map<String, MediaStorageStrategy> originalStrategies;

	@Mock
	private MediaStorageStrategy dummyFileStrategy;

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);

		fooBarStorage.switchToValue(DUMMY_MEDIA_STORAGE_STRATEGY_ID);

		catalog = this.modelService.create(CatalogModel.class);
		catalog.setId("my_catalog");
		modelService.save(catalog);

		catalogVersion = modelService.create(CatalogVersionModel.class);
		catalogVersion.setVersion("my_version");
		catalogVersion.setCatalog(catalog);
		modelService.save(catalogVersion);

		sampleFolder = prepareMediaFolder();
		sampleFolderConfig = mediaStorageConfigService.getConfigForFolder(sampleFolder.getQualifier());
		media1 = prepareMedia("media1", sampleFolder);
		media2 = prepareMedia("media2", sampleFolder);
		media3 = prepareMedia("media3", sampleFolder);

		modelService.saveAll(media1, media2, media3);

		originalStrategies = defaultMediaStorageRegistry.getStorageStrategies();
		final Map<String, MediaStorageStrategy> storageStrategies = new HashMap<String, MediaStorageStrategy>(originalStrategies);
		storageStrategies.put(DUMMY_MEDIA_STORAGE_STRATEGY_ID, dummyFileStrategy);
		defaultMediaStorageRegistry.setStorageStrategies(storageStrategies);
	}

	@After
	public void tearDown() throws Exception
	{
		defaultMediaStorageRegistry.setStorageStrategies(originalStrategies);
		preventLongfilenamesSwitcher.switchBackToDefault();
		fooBarStorage.switchBackToDefault();

		cleanCacheFolder(sampleFolder.getQualifier());
		getMediaCacheRegion().clearCache();
		cleanCache();
	}

	private void cleanCacheFolder(final String folderName) throws IOException
	{
		Files.walkFileTree(Paths.get(System.getProperty("java.io.tmpdir"), folderName), new SimpleFileVisitor<Path>()
		{
			@Override
			public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException
			{
				if (Files.exists(file))
				{
					Files.delete(file);
				}
				return FileVisitResult.CONTINUE;
			}

			@Override
			public FileVisitResult visitFileFailed(final Path file, final IOException exc) throws IOException
			{
				if (Files.exists(file))
				{
					Files.delete(file);
				}
				return FileVisitResult.CONTINUE;
			}
		});
	}

	private MediaModel prepareMedia(final String code, final MediaFolderModel folder)
	{
		final MediaModel media = modelService.create(MediaModel.class);
		media.setCode(code);
		media.setCatalogVersion(catalogVersion);
		media.setFolder(folder);

		return media;
	}

	private MediaFolderModel prepareMediaFolder()
	{
		final MediaFolderModel folder = modelService.create(MediaFolderModel.class);
		folder.setQualifier("fooBar");
		folder.setPath("fooBar");
		modelService.save(folder);

		return folder;
	}

	private void cleanCache()
	{
		try
		{
			final CacheRegion mediaCacheRegion = getMediaCacheRegion();
			final Collection<CacheKey> allKeys = mediaCacheRegion.getAllKeys();
			for (final CacheKey cacheKey : allKeys)
			{
				if (TENANT_ID.equalsIgnoreCase(cacheKey.getTenantId()))
				{
					mediaCacheRegion.remove(cacheKey, false);
				}
			}

			FileUtils.deleteDirectory(new File(MediaUtil.getLocalStorageDataDir() + "/cache"));
		}
		catch (final IOException e)
		{
			LOG.error("Cannot clean out testing cache directory");
		}
	}

	private CacheRegion getMediaCacheRegion()
	{
		final Optional<CacheRegion> mediaCacheRegion = Iterables.tryFind(cacheController.getRegions(), new Predicate<CacheRegion>()
		{

			@Override
			public boolean apply(@Nullable
			final CacheRegion input)
			{
				return (input instanceof MediaCacheRegion);
			}
		});
		assertThat(mediaCacheRegion.isPresent()).isTrue();
		return mediaCacheRegion.get();
	}

	@Test
	public void shouldThrowExceptionWhenFilenameTooLong()
	{
		preventLongfilenamesSwitcher.switchToValue("false");

		given(dummyFileStrategy.store(Mockito.any(), Mockito.anyString(), Mockito.any(), Mockito.any()))
				.willReturn(new StoredMediaData(PATH + LONG_FILENAME + JPG_EXTENSION, "hash", 1, IMAGE_JPEG));

		final byte[] randomBytes = getRandomBytes();

		try
		{
			mediaService.setStreamForMedia(media3, getSampleInputStream(randomBytes), LONG_FILENAME + JPG_EXTENSION, IMAGE_JPEG);
			fail("ModelSavingException expected");
		}
		catch (final ModelSavingException e)
		{
			assertThat(e.getMessage()).contains("Error writing media file");
		}
	}

	@Test
	public void shouldCacheFilesWithLongNamesAndAvoidNameColissions() throws Exception
	{
		final String first = "_first";
		final String second = "_second";

		preventLongfilenamesSwitcher.switchToValue("true");

		final byte[] randomBytes1 = getRandomBytes();
		final byte[] randomBytes2 = getRandomBytes();

		given(dummyFileStrategy.store(Mockito.any(), Mockito.anyString(), Mockito.any(), Mockito.any()))
				.willReturn(new StoredMediaData(PATH + LONG_FILENAME + first + JPG_EXTENSION, "hash2", 1, IMAGE_JPEG));
		mediaService.setStreamForMedia(media1, getSampleInputStream(randomBytes1), LONG_FILENAME + first + JPG_EXTENSION,
				IMAGE_JPEG);

		given(dummyFileStrategy.store(Mockito.any(), Mockito.anyString(), Mockito.any(), Mockito.any()))
				.willReturn(new StoredMediaData(PATH + LONG_FILENAME + second + JPG_EXTENSION, "hash2", 1, IMAGE_JPEG));
		mediaService.setStreamForMedia(media2, getSampleInputStream(randomBytes2), LONG_FILENAME + second + JPG_EXTENSION,
				IMAGE_JPEG);

		assertThat(mediaService.getFiles(media1)).isNotEmpty().hasSize(1);
		assertThat(mediaService.getFiles(media2)).isNotEmpty().hasSize(1);

		final Object cacheObject1 = getMediaCacheRegion().get(new DefaultLocalMediaFileCacheService.MediaCacheKey(TENANT_ID,
				CACHE_FOLDER, PATH + LONG_FILENAME + first + JPG_EXTENSION));

		final Object cacheObject2 = getMediaCacheRegion().get(new DefaultLocalMediaFileCacheService.MediaCacheKey(TENANT_ID,
				CACHE_FOLDER, PATH + LONG_FILENAME + second + JPG_EXTENSION));

		assertThat(cacheObject1).isInstanceOf(MediaCacheUnit.class);
		assertThat(cacheObject2).isInstanceOf(MediaCacheUnit.class);

		assertThat(new FileInputStream(((MediaCacheUnit) cacheObject1).getFile())).hasSameContentAs(randomBytes1);
		assertThat(new FileInputStream(((MediaCacheUnit) cacheObject2).getFile())).hasSameContentAs(randomBytes2);
	}

	@Test
	public void shouldRecreateCacheFromExistingCachedFileEntriesUsingDefaultCacheFolderWhithoutNonRestorable()
	{
		// given
		preventLongfilenamesSwitcher.switchToValue("true");
		final List<MediaFolderConfig> configs = Collections.emptyList();

		given(dummyFileStrategy.store(Mockito.any(), Mockito.anyString(), Mockito.any(), Mockito.any()))
				.willReturn(new StoredMediaData(PATH + LONG_FILENAME + JPG_EXTENSION, "hash1", 1, IMAGE_JPEG));
		mediaService.setStreamForMedia(media1, getSampleInputStream(getRandomBytes()), LONG_FILENAME + JPG_EXTENSION, IMAGE_JPEG);

		final Object cacheObject = getMediaCacheRegion().get(
				new DefaultLocalMediaFileCacheService.MediaCacheKey(TENANT_ID, CACHE_FOLDER, PATH + LONG_FILENAME + JPG_EXTENSION));
		assertThat(cacheObject).isInstanceOf(MediaCacheUnit.class);
		final File file = ((MediaCacheUnit) cacheObject).getFile();
		assertThat(file).exists();

		given(dummyFileStrategy.store(Mockito.any(), Mockito.anyString(), Mockito.any(), Mockito.any()))
				.willReturn(new StoredMediaData(PATH + "shortname" + JPG_EXTENSION, "hash2", 1, IMAGE_JPEG));
		mediaService.setStreamForMedia(media2, getSampleInputStream(getRandomBytes()), "shortname" + JPG_EXTENSION, IMAGE_JPEG);

		// when
		getMediaCacheRegion().clearCache();
		localMediFileCacheRecreator.recreateCache(mediaStorageConfigService.getDefaultCacheFolderName(), configs);

		// then
		assertThat(getMediaCacheRegion().getMaxReachedSize()).isEqualTo(1);
		assertThat(file).doesNotExist();
	}

	private byte[] getRandomBytes()
	{
		return RandomStringUtils.randomAlphabetic(1024).getBytes();
	}

	private InputStream getSampleInputStream(final byte[] data)
	{
		return new DataInputStream(new ByteArrayInputStream(data));
	}
}
