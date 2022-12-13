/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.impex;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.CatalogUnawareMediaModel;
import de.hybris.platform.core.model.media.MediaFolderModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.jalo.user.UserManager;
import de.hybris.platform.servicelayer.ServicelayerTest;
import de.hybris.platform.servicelayer.impex.impl.StreamBasedImpExResource;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.testframework.TestUtils;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.base.Charsets;


@Transactional
@IntegrationTest
public class MediaEncodingIntegrationTest extends ServicelayerTest
{

	@Resource
	ModelService modelService;

	@Resource
	MediaService mediaService;

	@Resource
	ImportService importService;

	private final String validFilename = "C.PNG";
	private final String invalidFilename = "\u00C7.PNG";
	private final String someContent = "some content";
	private final PropertyConfigSwitcher zipFilenameEncodings = new PropertyConfigSwitcher("zip.filename.encodings");

	@Before
	public void setUp() throws Exception
	{
		JaloSession.getCurrentSession().setUser(UserManager.getInstance().getAdminEmployee());
		createCoreData();
		createDefaultCatalog();
	}

	@Test
	public void shouldNotFailOnZipFileWithFilenameEncondingsFromList() throws IOException
	{
		zipFilenameEncodings.switchToValue("utf8,cp852");
		final ImportResult importResult = createAndImportZipFileWithGivenEncoding("cp852");
		/*
		 * Check the csv file for detailed message:
		 * importResult.getUnresolvedLines().getLocation();
		 * Output:
		 * Media,,,,malformed input off : 46, length : 1
		 */

		assertThat(importResult.isSuccessful()).isTrue();

		MediaModel model = mediaService.getMedia("med1");
		assertThat(model).isNotNull();
		assertThat(model.getRealFileName()).isEqualTo(validFilename);

		byte[] data = mediaService.getDataFromMedia(model);
		assertThat(new String(data)).isEqualTo(someContent);

		model = mediaService.getMedia("med2");
		assertThat(model).isNotNull();
		assertThat(model.getRealFileName()).isEqualTo(invalidFilename);

		data = mediaService.getDataFromMedia(model);
		assertThat(new String(data)).isEqualTo(someContent);
	}

	@Test
	public void shouldFailOnZipFilenameEncodingOutsideOfList() throws IOException
	{
		zipFilenameEncodings.switchToValue("utf8");
		TestUtils.disableFileAnalyzer("expecting exeption");
		final ImportResult importResult = createAndImportZipFileWithGivenEncoding("cp852");

		assertThat(importResult.isSuccessful()).isFalse();
	}

	private ImportResult createAndImportZipFileWithGivenEncoding(final String encoding) throws IOException
	{
		final File f = File.createTempFile("file", "zip");
		final ZipOutputStream out = new ZipOutputStream(new FileOutputStream(f), Charset.forName(encoding));
		ZipEntry myEntry = new ZipEntry(validFilename);
		final StringBuilder sb = new StringBuilder(someContent);
		final byte[] data = sb.toString().getBytes();
		out.putNextEntry(myEntry);
		out.write(data, 0, data.length);
		out.closeEntry();

		myEntry = new ZipEntry(invalidFilename);
		out.putNextEntry(myEntry);
		out.write(data, 0, data.length);
		out.closeEntry();

		out.close();


		final Path path = f.toPath();
		final MediaModel media = createMedia(Files.readAllBytes(path), path.getFileName().toString(),
				  "application/x-zip-compressed");
		final ImportConfig importConfig = createImportConfig(media);
		return importService.importData(importConfig);
	}

	protected MediaModel createMedia(final byte[] data, final String fileName, final String contentType)
	{
		final CatalogUnawareMediaModel mediaModel = modelService.create(CatalogUnawareMediaModel._TYPECODE);
		mediaModel.setCode(generateId(fileName));
		final MediaFolderModel importFolder = mediaService.getFolder("root");
		mediaModel.setFolder(importFolder);
		mediaModel.setRealFileName(fileName);
		mediaModel.setMime(contentType);
		modelService.save(mediaModel);
		mediaService.setDataForMedia(mediaModel, data);
		return mediaModel;
	}

	protected String generateId(final String fileName)
	{
		return String.format("%s%s%s", "TEST", fileName.replaceAll("[^0-9a-zA-Z]", ""), UUID.randomUUID().getMostSignificantBits());
	}

	protected ImportConfig createImportConfig(final MediaModel mediaModel)
	{
		final ImportConfig config = new ImportConfig();
		config.setFailOnError(true);
		final Charset charset = Charset.forName("Cp852");
		final ImpExResource resource = new StreamBasedImpExResource(
				  new ByteArrayInputStream(getScript().getBytes(charset)), charset.name());
		config.setScript(resource);
		config.setEnableCodeExecution(Boolean.FALSE);
		config.setHmcSavedValuesEnabled(true);

		config.setMediaArchive(new StreamBasedImpExResource(mediaService.getStreamFromMedia(mediaModel), Charsets.UTF_8.name()));

		return config;
	}

	protected String getScript()
	{
		return "INSERT_UPDATE Media;code[unique=true];catalogVersion(version,catalog(id))[unique=true];folder(qualifier);@media[translator=de.hybris.platform.impex.jalo.media.MediaDataTranslator];\n"
				  + ";med1;Online:testCatalog;root;" + validFilename + ";\n"
				  + ";med2;Online:testCatalog;root;" + invalidFilename + ";\n";
	}

	@After
	public void TearDown() throws Exception
	{
		zipFilenameEncodings.switchBackToDefault();
	}
}
