/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.media.storage.impl;

import static de.hybris.platform.jalo.media.MediaManager.ROOT_FOLDER_QUALIFIER;
import static de.hybris.platform.media.storage.impl.DefaultMediaStorageConfigService.DefaultSettingKeys.HASHING_DEPTH_KEY;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.media.MediaFolderModel;
import de.hybris.platform.media.exceptions.MediaInvalidLocationException;
import de.hybris.platform.media.storage.impl.LocalFileMediaStorageStrategy.LocalFileMediaVerifier;
import de.hybris.platform.servicelayer.exceptions.AmbiguousIdentifierException;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.media.MediaService;

import java.util.Arrays;
import java.util.Collections;

import org.assertj.core.api.Assertions;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@UnitTest
public class LocalFileMediaVerifierTest
{
	private static final Logger LOG = LoggerFactory.getLogger(LocalFileMediaVerifierTest.class);

	private final String[][] validCases = new String[][]
	{
			{ ROOT_FOLDER_QUALIFIER, null, "h1/h2/ddd.jpg" },
			{ ROOT_FOLDER_QUALIFIER, null, "/h1/h2/ddd.jpg" },
			{ ROOT_FOLDER_QUALIFIER, null, "ala/../h1/h2/ddd.jpg" },
			{ ROOT_FOLDER_QUALIFIER, null, "./h1/h2/ddd.jpg" },
			{ ROOT_FOLDER_QUALIFIER, null, "/./h1/h2/ddd.jpg" },
			{ "test", "test", "test/h1/h2/ddd.jpg" },
			{ "test", "test", "./test/h1/h2/ddd.jpg" },
			{ "test", "test/tes", "test/tes/h1/h2/ddd.jpg" },
			{ "test", "test/tes/te", "test/tes/te/h1/h2/ddd.jpg" },
			{ "test", "test", "test/./h1/h2/ddd.jpg" },
			{ "test", "test", "dddd/../test/h1/h2/ddd.jpg" }
	};

	private final Object[][] invalidCases = new Object[][]
	{
			{ ROOT_FOLDER_QUALIFIER, null, "test/h1/h2/ddd.jpg" },
			{ "test", "test", "" },
			{ "test", "test", "ddd.jpg" },
			{ "test", "test", "h2/ddd.jpg" },
			{ "test", "test", "/h2/ddd.jpg" },
			{ "test", "test", "h1/h2/ddd.jpg" },
			{ "test", "test", "/h1/h2/ddd.jpg" },
			{ "test", "test", "other/h1/h2/ddd.jpg" },
			{ "test", "test", "test/../other/h1/h2/ddd.jpg" },
			{ "test", "test/tes", "" },
			{ "test", "test/tes", "ddd.jpg" },
			{ "test", "test/tes", "h2/ddd.jpg" },
			{ "test", "test/tes", "/h2/ddd.jpg" },
			{ "test", "test/tes", "h1/h2/ddd.jpg" },
			{ "test", "test/tes", "/h1/h2/ddd.jpg" },
			{ "test", "test/tes", "other/h1/h2/ddd.jpg" },
			{ "test", "test/tes", "test/tes/../other/h1/h2/ddd.jpg" },
			{ "test", UnknownIdentifierException.class, "test/h1/h2/ddd.jpg" },
			{ "test", AmbiguousIdentifierException.class, "test/h1/h2/ddd.jpg" },
			{ null, "test", "test/h1/h2/ddd.jpg" },
	};

	@Test
	public void shouldNotFailOnValidCases()
	{
		for (final String[] validCase : validCases)
		{
			final LocalFileMediaVerifier verifier = givenVerifier(validCase[0], validCase[1], validCase[2]);
			LOG.info("Verifying valid case {}", Arrays.toString(validCase));
			verifier.verifyFolderAndLocationIntegrity();
		}
	}

	@Test
	public void shouldFailOnInvalidCases()
	{
		for (final Object[] invalidCase : invalidCases)
		{
			final LocalFileMediaVerifier verifier = givenVerifier((String) invalidCase[0], invalidCase[1],
					(String) invalidCase[2]);
			LOG.info("Verifying invalid case {}", Arrays.toString(invalidCase));

			Assertions.assertThatExceptionOfType(MediaInvalidLocationException.class)
			          .isThrownBy(verifier::verifyFolderAndLocationIntegrity);
		}
	}

	private LocalFileMediaVerifier givenVerifier(final String folderQualifier, final Object folderPath, final String location)
	{
		final DefaultMediaFolderConfig config = new DefaultMediaFolderConfig(folderQualifier,
				Collections.singletonMap(HASHING_DEPTH_KEY.getKey(), 2));
		return new LocalFileMediaVerifier(mockMediaService(folderQualifier, folderPath), config, location);
	}

	private MediaService mockMediaService(final String folderQualifier, final Object folderPath)
	{
		final MediaService mediaService = mock(MediaService.class);
		if (folderPath instanceof Class && RuntimeException.class.isAssignableFrom((Class<?>) folderPath))
		{
			when(mediaService.getFolder(folderQualifier)).thenThrow((Class<RuntimeException>) folderPath);
			return mediaService;
		}
		if ((folderPath == null || folderPath instanceof String) && folderQualifier != null)
		{
			final MediaFolderModel folder = mock(MediaFolderModel.class);
			when(folder.getPath()).thenReturn((String) folderPath);
			when(mediaService.getFolder(folderQualifier)).thenReturn(folder);
			return mediaService;
		}
		when(mediaService.getFolder(any())).thenThrow(UnsupportedOperationException.class);
		return mediaService;
	}
}