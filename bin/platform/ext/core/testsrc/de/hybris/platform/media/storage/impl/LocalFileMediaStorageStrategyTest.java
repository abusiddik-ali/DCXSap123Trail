/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.media.storage.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.jalo.media.MediaManager;
import de.hybris.platform.media.exceptions.MediaInvalidLocationException;
import de.hybris.platform.media.exceptions.MediaNotFoundException;
import de.hybris.platform.media.exceptions.MediaStoreException;
import de.hybris.platform.media.services.MediaLocationHashService;
import de.hybris.platform.media.services.MediaStorageInitializer;
import de.hybris.platform.media.services.MimeService;
import de.hybris.platform.media.storage.MediaMetaData;
import de.hybris.platform.media.storage.MediaStorageConfigService;
import de.hybris.platform.media.storage.MediaStorageConfigService.MediaFolderConfig;
import de.hybris.platform.media.storage.MediaStorageStrategy;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.test.TestThreadsHolder;
import de.hybris.platform.util.MediaUtil;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import javax.annotation.Resource;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;


@IntegrationTest
public class LocalFileMediaStorageStrategyTest extends ServicelayerBaseTest
{
	private static final String MEDIA_ID = "123456";
	private static final String REAL_FILENAME = "foo.jpg";
	private static final String MIME = "image/jpeg";
	private static final String FOLDER_FOO_PATH = "foo";
	private static final String FOLDER_FOO_QUALIFIER = FOLDER_FOO_PATH;
	private static final String FOLDER_FOO_SECURED_PATH = "foosecured";
	private static final String FOLDER_FOO_SECURED_QUALIFIER = FOLDER_FOO_SECURED_PATH;
	private static final String FOLDER_BAR_PATH = "bar";
	private static final String FOLDER_BAR_QUALIFIER = FOLDER_BAR_PATH;
	private static final String FOLDER_WITH_SLASH_PATH = "with/slash";
	private static final String FOLDER_WITH_SLASH_QUALIFIER = "slash";
	private static final String FOLDER_WITH_PERCENT_PATH = "%oo";
	private static final String FOLDER_WITH_PERCENT_QUALIFIER = "percent";
	private static final String PROPER_LOCATION = "/he6/hf4/";
	private static final String PROPER_LOCATION_FOO = FOLDER_FOO_PATH + PROPER_LOCATION;
	private static final String PROPER_LOCATION_FOOSECURED = FOLDER_FOO_SECURED_PATH + PROPER_LOCATION;
	private static final String PROPER_LOCATION_WITH_SLASH = FOLDER_WITH_SLASH_PATH + PROPER_LOCATION;


	@Resource(name = "mediaStorageConfigService")
	private MediaStorageConfigService storageConfigService;
	@Resource(name = "localFileMediaStorageStrategy")
	private MediaStorageStrategy mediaStorageStrategy;
	@Resource(name = "localFileMediaStorageCleaner")
	private MediaStorageInitializer mediaStorageCleaner;
	@Resource
	private MimeService mimeService;
	@Resource
	MediaLocationHashService mediaLocationHashService;
	@Mock
	private InputStream inputStream;
	private final File tempStorage = MediaUtil.getLocalStorageDataDir();
	private MediaFolderConfig folderFooConfig;
	private MediaFolderConfig folderFooSecuredConfig;
	private MediaFolderConfig folderBarConfig;
	private MediaFolderConfig folderWithSlashConfig;
	private MediaFolderConfig folderWithPercentConfig;

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);
		assertThat(mediaStorageStrategy).isNotNull();
		MediaManager.getInstance().createMediaFolder(FOLDER_FOO_QUALIFIER, FOLDER_FOO_PATH);
		MediaManager.getInstance().createMediaFolder(FOLDER_FOO_SECURED_QUALIFIER, FOLDER_FOO_SECURED_PATH);
		MediaManager.getInstance().createMediaFolder(FOLDER_BAR_QUALIFIER, FOLDER_BAR_PATH);
		MediaManager.getInstance().createMediaFolder(FOLDER_WITH_SLASH_QUALIFIER, FOLDER_WITH_SLASH_PATH);
		MediaManager.getInstance().createMediaFolder(FOLDER_WITH_PERCENT_QUALIFIER, FOLDER_WITH_PERCENT_PATH);
		folderFooConfig = storageConfigService.getConfigForFolder(FOLDER_FOO_QUALIFIER);
		folderFooSecuredConfig = storageConfigService.getConfigForFolder(FOLDER_FOO_SECURED_QUALIFIER);
		folderBarConfig = storageConfigService.getConfigForFolder(FOLDER_BAR_QUALIFIER);
		folderWithSlashConfig = storageConfigService.getConfigForFolder(FOLDER_WITH_SLASH_QUALIFIER);
		folderWithPercentConfig = storageConfigService.getConfigForFolder(FOLDER_WITH_SLASH_QUALIFIER);
	}

	@After
	public void cleanUp() throws Exception
	{
		mediaStorageCleaner.onInitialize();
	}

	@Test
	public void shouldStoreFileInAllReplicationDirs() throws IOException
	{
		final String tempDir = System.getProperty("java.io.tmpdir");

		final File dataDir = new File(tempDir, "_test_datadir");
		dataDir.mkdirs();
		final File replicationDir1 = new File(tempDir, "_test_replicationdir1");
		replicationDir1.mkdirs();
		final File replicationDir2 = new File(tempDir, "_test_replicationdir2");
		replicationDir2.mkdirs();

		final LocalFileMediaStorageStrategy lfStrategy = new LocalFileMediaStorageStrategy();
		lfStrategy.setMainDataDir(dataDir);
		// !!! despite the name 'setReplicationDirs()' must include the main data dir as well !!!
		lfStrategy.setReplicationDirs(Arrays.asList(dataDir, replicationDir1, replicationDir2));
		lfStrategy.setMimeService(mimeService);
		lfStrategy.setLocationHashService(mediaLocationHashService);


		// given
		final byte[] rawData = "AllReplicationDirsShouldGetTheSameDataTest!!!".getBytes();
		final Map<String, Object> metaData = buildMediaMetaData(MIME, REAL_FILENAME, FOLDER_FOO_PATH);

		final File expectedDataFile = new File(dataDir, PROPER_LOCATION_FOO + MEDIA_ID + ".jpg");
		final File expectedRepFile1 = new File(replicationDir1, PROPER_LOCATION_FOO + MEDIA_ID + ".jpg");
		final File expectedRepFile2 = new File(replicationDir2, PROPER_LOCATION_FOO + MEDIA_ID + ".jpg");
		try
		{

			// when
			final StoredMediaData storedMedia = lfStrategy
					.store(folderFooConfig, MEDIA_ID, metaData, new ByteArrayInputStream(rawData));

			// then
			assertThat(storedMedia).isNotNull();
			assertThat(storedMedia.getLocation()).isNotNull();
			assertThat(storedMedia.getLocation()).isEqualTo(PROPER_LOCATION_FOO + MEDIA_ID + ".jpg");

			assertThat(expectedDataFile.exists()).isTrue();
			assertThat(FileUtils.readFileToByteArray(expectedDataFile)).isEqualTo(rawData);

			assertThat(expectedRepFile1.exists()).isTrue();
			assertThat(FileUtils.readFileToByteArray(expectedRepFile1)).isEqualTo(rawData);

			assertThat(expectedRepFile2.exists()).isTrue();
			assertThat(FileUtils.readFileToByteArray(expectedRepFile2)).isEqualTo(rawData);
		}
		finally
		{
			FileUtils.deleteQuietly(expectedDataFile);
			FileUtils.deleteQuietly(expectedRepFile1);
			FileUtils.deleteQuietly(expectedRepFile2);
		}
	}

	@Test
	public void shouldStoreFileInLocalStorageAndReturnStoredMediaDataObjectWithStorageLocationAndSize() throws Exception
	{
		// given
		given(Integer.valueOf(inputStream.read(any(byte[].class)))).willReturn(Integer.valueOf(1), Integer.valueOf(0),
				Integer.valueOf(-1));
		final Map<String, Object> metaData = buildMediaMetaData(MIME, REAL_FILENAME, FOLDER_FOO_PATH);

		// when
		final StoredMediaData storedMedia = mediaStorageStrategy.store(folderFooConfig, MEDIA_ID, metaData, inputStream);

		// then
		assertThat(storedMedia).isNotNull();
		assertThat(storedMedia.getLocation()).isNotNull();
		assertThat(storedMedia.getLocation()).isEqualTo(PROPER_LOCATION_FOO + MEDIA_ID + ".jpg");
		assertThat(new File(tempStorage, PROPER_LOCATION_FOO + MEDIA_ID + ".jpg").exists()).isTrue();
	}

	@Test
	public void shouldThrowMediaStoreExceptionWhenFileWithTheSameNameAlreadyExist() throws Exception
	{
		// given
		given(Integer.valueOf(inputStream.read(any(byte[].class)))).willReturn(Integer.valueOf(1), Integer.valueOf(0),
				Integer.valueOf(-1));
		final Map<String, Object> metaData = buildMediaMetaData(MIME, REAL_FILENAME, FOLDER_FOO_PATH);

		try
		{
			// when
			mediaStorageStrategy.store(folderFooConfig, MEDIA_ID, metaData, inputStream);
			// try to write again the same file
			mediaStorageStrategy.store(folderFooConfig, MEDIA_ID, metaData, inputStream);
			fail("Shoud throw MediaStoreException");
		}
		catch (final MediaStoreException e)
		{
			final String mediaDirPath = new File(tempStorage, PROPER_LOCATION_FOO).getAbsolutePath();
			// then
			assertThat(e.getMessage()).startsWith("New media file already exists! (mediaId: 123456, file:");
			assertThat(e.getMessage()).endsWith(", dir: " + mediaDirPath + ")");
		}
	}

	@Test
	public void shouldRemoveMediaIfMediaExists() throws Exception
	{
		// given
		given(Integer.valueOf(inputStream.read(any(byte[].class)))).willReturn(Integer.valueOf(1), Integer.valueOf(0),
				Integer.valueOf(-1));
		final Map<String, Object> metaData = buildMediaMetaData(MIME, REAL_FILENAME, FOLDER_FOO_PATH);
		final StoredMediaData storeMedia = mediaStorageStrategy.store(folderFooConfig, MEDIA_ID, metaData, inputStream);
		assertThat(new File(tempStorage, PROPER_LOCATION_FOO + MEDIA_ID + ".jpg").exists()).isTrue();

		// when
		mediaStorageStrategy.delete(folderFooConfig, storeMedia.getLocation());

		// then
		assertThat(new File(tempStorage, PROPER_LOCATION_FOO + MEDIA_ID + ".jpg").exists()).isFalse();
	}

	@Test
	public void shouldNotThrowMediaRemovalExceptionWhenMediaAlreadyRemovedByOtherThread() throws Exception
	{
		// given
		given(Integer.valueOf(inputStream.read(any(byte[].class)))).willReturn(Integer.valueOf(1), Integer.valueOf(0),
				Integer.valueOf(-1));
		final Map<String, Object> metaData = buildMediaMetaData(MIME, REAL_FILENAME, FOLDER_FOO_PATH);
		final StoredMediaData storeMedia = mediaStorageStrategy.store(folderFooConfig, MEDIA_ID, metaData, inputStream);
		assertThat(new File(tempStorage, PROPER_LOCATION_FOO + MEDIA_ID + ".jpg").exists()).isTrue();

		// when
		final TestThreadsHolder<Runnable> threads = new TestThreadsHolder<>(10,
				() -> mediaStorageStrategy.delete(folderFooConfig,
						storeMedia.getLocation()));
		threads.startAll();
		threads.waitForAll(4, TimeUnit.SECONDS);


		// then
		assertThat(new File(tempStorage, PROPER_LOCATION_FOO + MEDIA_ID + ".jpg").exists()).isFalse();
		assertThat(threads.getErrors()).isEmpty();
	}

	@Test
	public void shouldGetMediaAsStream() throws Exception
	{
		// given
		given(Integer.valueOf(inputStream.read(any(byte[].class)))).willReturn(Integer.valueOf(1), Integer.valueOf(0),
				Integer.valueOf(-1));
		final Map<String, Object> metaData = buildMediaMetaData(MIME, REAL_FILENAME, FOLDER_FOO_PATH);
		final StoredMediaData storeMedia = mediaStorageStrategy.store(folderFooConfig, MEDIA_ID, metaData, inputStream);
		assertThat(new File(tempStorage, PROPER_LOCATION_FOO + MEDIA_ID + ".jpg").exists()).isTrue();
		InputStream mediaAsStream = null;

		try
		{
			// when
			mediaAsStream = mediaStorageStrategy.getAsStream(folderFooConfig, storeMedia.getLocation());

			// then
			assertThat(mediaAsStream).isNotNull();
		}
		finally
		{
			if (mediaAsStream != null)
			{
				IOUtils.closeQuietly(mediaAsStream);
			}
		}
	}

	@Test
	public void shouldNotGetMediaFromFolderWithPercentAsStream() throws Exception
	{
		// given
		given(Integer.valueOf(inputStream.read(any(byte[].class)))).willReturn(Integer.valueOf(1), Integer.valueOf(0),
				Integer.valueOf(-1));
		final Map<String, Object> metaData = buildMediaMetaData(MIME, REAL_FILENAME, FOLDER_FOO_PATH);
		final StoredMediaData storeMedia = mediaStorageStrategy.store(folderFooConfig, MEDIA_ID, metaData, inputStream);
		assertThat(new File(tempStorage, PROPER_LOCATION_FOO + MEDIA_ID + ".jpg").exists()).isTrue();
		InputStream mediaAsStream = null;

		try
		{
			// when
			mediaAsStream = mediaStorageStrategy.getAsStream(folderWithPercentConfig, storeMedia.getLocation());

			// then
			fail("Should throw MediaInvalidLocationException");
		}
		catch (final MediaInvalidLocationException mile)
		{
			// fine
		}
		finally
		{
			if (mediaAsStream != null)
			{
				IOUtils.closeQuietly(mediaAsStream);
			}
		}
	}

	@Test
	public void shouldGetMediaFromFolderWithSlashAsStream() throws Exception
	{
		// given
		given(Integer.valueOf(inputStream.read(any(byte[].class)))).willReturn(Integer.valueOf(1), Integer.valueOf(0),
				Integer.valueOf(-1));
		final Map<String, Object> metaData = buildMediaMetaData(MIME, REAL_FILENAME, FOLDER_WITH_SLASH_PATH);
		final StoredMediaData storeMedia = mediaStorageStrategy.store(folderWithSlashConfig, MEDIA_ID, metaData, inputStream);
		assertThat(new File(tempStorage, PROPER_LOCATION_WITH_SLASH + MEDIA_ID + ".jpg").exists()).isTrue();
		InputStream mediaAsStream = null;

		try
		{
			// when
			mediaAsStream = mediaStorageStrategy.getAsStream(folderWithSlashConfig, storeMedia.getLocation());

			// then
			assertThat(mediaAsStream).isNotNull();
		}
		finally
		{
			if (mediaAsStream != null)
			{
				IOUtils.closeQuietly(mediaAsStream);
			}
		}
	}

	@Test
	public void shouldNotGetMediaFromTamperedLocationAsStream() throws Exception
	{
		// given
		given(Integer.valueOf(inputStream.read(any(byte[].class)))).willReturn(Integer.valueOf(1), Integer.valueOf(0),
				Integer.valueOf(-1));
		final Map<String, Object> metaData = buildMediaMetaData(MIME, REAL_FILENAME, FOLDER_FOO_PATH);
		final StoredMediaData storeMedia = mediaStorageStrategy.store(folderFooConfig, MEDIA_ID, metaData, inputStream);
		assertThat(new File(tempStorage, PROPER_LOCATION_FOO + MEDIA_ID + ".jpg").exists()).isTrue();
		InputStream mediaAsStream = null;

		try
		{

			// when
			mediaAsStream = mediaStorageStrategy.getAsStream(storageConfigService.getConfigForFolder("bar"), storeMedia.getLocation());

			// then
			fail("Should throw MediaInvalidLocationException");
		}
		catch (final MediaInvalidLocationException mile)
		{
			// fine
		}
		finally
		{
			if (mediaAsStream != null)
			{
				IOUtils.closeQuietly(mediaAsStream);
			}
		}
	}

	@Test
	public void shouldNotGetMediaFromTamperedLocationAsRootFolderAsStream() throws Exception
	{
		// given
		given(Integer.valueOf(inputStream.read(any(byte[].class)))).willReturn(Integer.valueOf(1), Integer.valueOf(0),
				Integer.valueOf(-1));
		final Map<String, Object> metaData = buildMediaMetaData(MIME, REAL_FILENAME, FOLDER_FOO_SECURED_PATH);
		final StoredMediaData storeMedia = mediaStorageStrategy.store(folderFooSecuredConfig, MEDIA_ID, metaData, inputStream);
		assertThat(new File(tempStorage, PROPER_LOCATION_FOOSECURED + MEDIA_ID + ".jpg").exists()).isTrue();
		InputStream mediaAsStream = null;

		try
		{

			// when
			mediaAsStream = mediaStorageStrategy.getAsStream(storageConfigService.getConfigForFolder(MediaManager.ROOT_FOLDER_QUALIFIER), storeMedia.getLocation());

			// then
			fail("Should throw MediaInvalidLocationException");
		}
		catch (final MediaInvalidLocationException mile)
		{
			// fine
		}
		finally
		{
			if (mediaAsStream != null)
			{
				IOUtils.closeQuietly(mediaAsStream);
			}
		}
	}

	@Test
	public void shouldReturnSizeOfAnObjectInStorage() throws Exception
	{
		// given
		given(Integer.valueOf(inputStream.read(any(byte[].class)))).willReturn(Integer.valueOf(1), Integer.valueOf(0),
				Integer.valueOf(-1));
		final Map<String, Object> metaData = buildMediaMetaData(MIME, REAL_FILENAME, FOLDER_FOO_PATH);
		final StoredMediaData storeMedia = mediaStorageStrategy.store(folderFooConfig, MEDIA_ID, metaData, inputStream);
		assertThat(new File(tempStorage, PROPER_LOCATION_FOO + MEDIA_ID + ".jpg").exists()).isTrue();

		// when
		final long size = mediaStorageStrategy.getSize(folderFooConfig, storeMedia.getLocation());

		// then
		assertThat(size).isEqualTo(1);
	}

	@Test
	public void shouldThrowMediaNotFoundExceptionWhenAskingForSizeForNonExistentObject() throws Exception
	{
		// given
		final String mediaLocation = FOLDER_FOO_PATH + "/h21/h21/NON_EXISISTENT";

		try
		{
			// when
			mediaStorageStrategy.getSize(folderFooConfig, mediaLocation);
			fail("Should throw MediaNotFoundException");

		}
		catch (final MediaNotFoundException e)
		{
			// then fine
		}
	}

	@Test
	public void shouldGetMediaAsFile() throws Exception
	{
		// given
		given(Integer.valueOf(inputStream.read(any(byte[].class)))).willReturn(Integer.valueOf(1), Integer.valueOf(0),
				Integer.valueOf(-1));
		final Map<String, Object> metaData = buildMediaMetaData(MIME, REAL_FILENAME, FOLDER_FOO_PATH);
		final StoredMediaData storeMedia = mediaStorageStrategy.store(folderFooConfig, MEDIA_ID, metaData, inputStream);
		assertThat(new File(tempStorage, PROPER_LOCATION_FOO + MEDIA_ID + ".jpg").exists()).isTrue();

		// when
		final File mediaAsFile = mediaStorageStrategy.getAsFile(folderFooConfig, storeMedia.getLocation());

		// then
		assertThat(mediaAsFile).isNotNull();
	}

	private Map<String, Object> buildMediaMetaData(final String mime, final String originalName, final String folderPath)
	{
		final Map<String, Object> metaData = new HashMap<>();
		metaData.put(MediaMetaData.MIME, mime);
		metaData.put(MediaMetaData.FILE_NAME, originalName);
		metaData.put(MediaMetaData.FOLDER_PATH, folderPath);
		return metaData;
	}

}
