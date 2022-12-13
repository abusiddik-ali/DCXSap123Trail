/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.interceptor.impl;

import static de.hybris.platform.media.storage.MediaStorageConfigService.MediaFolderConfig;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.media.MediaFolderModel;
import de.hybris.platform.media.storage.MediaStorageConfigService;
import de.hybris.platform.media.storage.MediaStorageRegistry;
import de.hybris.platform.media.storage.MediaStorageStrategy;
import de.hybris.platform.servicelayer.exceptions.AmbiguousIdentifierException;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.media.MediaService;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;


/**
 * Unit test for a {@link MediaFolderValidator}'s logic.
 */
@UnitTest
public class MediaFolderValidatorTest
{
	private MediaFolderValidator validator;

	@Mock
	private InterceptorContext ctx;

	@Mock
	private MediaService mediaService;

	@Mock
	private MediaStorageRegistry mediaStorageRegistry;

	@Mock
	private MediaStorageStrategy storageStrategy;

	@Mock
	private MediaStorageConfigService mediaStorageConfigService;

	@Mock
	private MediaFolderConfig mediaFolderConfig;

	private static final String FOLDER_QUALIFIER = "test";

	@Before
	public void prepare()
	{
		MockitoAnnotations.initMocks(this);
		validator = new MediaFolderValidator();
		validator.setMediaService(mediaService);
		validator.setMediaStorageRegistry(mediaStorageRegistry);
		validator.setMediaStorageConfigService(mediaStorageConfigService);
		when(mediaStorageRegistry.getStorageStrategyForFolder(any())).thenReturn(storageStrategy);
		when(mediaStorageConfigService.getConfigForFolder(any())).thenReturn(mediaFolderConfig);
		when(storageStrategy.hasValidMediaFolderName(any())).thenReturn(true);
		when(mediaFolderConfig.isMediaFolderNameValidationEnabled()).thenReturn(true);
	}

	@Test
	public void testNullModel() throws InterceptorException
	{
		validator.onValidate(null, ctx);
	}

	@Test
	public void testOtherFolderModel() throws InterceptorException
	{
		final MediaFolderModel otherModel = new MediaFolderModel();
		otherModel.setQualifier("otherfooFolder");

		final MediaFolderModel model = new MediaFolderModel();
		model.setQualifier("fooFolder");

		when(mediaService.getFolder(eq("fooFolder"))).thenReturn(otherModel);

		validator.onValidate(model, ctx);
	}


	@Test
	public void testTheSameFolderModel() throws InterceptorException
	{
		//final MediaFolderModel otherModel = new MediaFolderModel();
		//otherModel.setQualifier("otherfooFolder");

		final MediaFolderModel model = new MediaFolderModel();
		model.setQualifier("fooFolder");

		when(mediaService.getFolder(eq("fooFolder"))).thenReturn(model);

		validator.onValidate(model, ctx);
	}

	@Test
	public void testOtherWithDuplicatedQualifierFolderModel() throws InterceptorException
	{
		final MediaFolderModel otherModel = new MediaFolderModel();
		otherModel.setQualifier("fooFolder");

		final MediaFolderModel model = new MediaFolderModel();
		model.setQualifier("fooFolder");

		when(mediaService.getFolder(eq("fooFolder"))).thenReturn(otherModel);
		try
		{
			validator.onValidate(model, ctx);
		}
		catch (final InterceptorException ie)
		{
			Assert.assertTrue(ie.getInterceptor() instanceof MediaFolderValidator);
			Assert.assertTrue(ie.getMessage().contains("MediaFolder with qualifier: fooFolder already exists in the system."));
		}
	}

	@Test
	public void testNotExistingFolderModel() throws InterceptorException
	{
		final MediaFolderModel model = new MediaFolderModel();
		model.setQualifier("fooFolder");

		when(mediaService.getFolder(eq("fooFolder"))).thenThrow(new UnknownIdentifierException("foo not found"));
		try
		{
			validator.onValidate(model, ctx);
		}
		catch (final InterceptorException ie)
		{
			Assert.fail("Not existing folder should not break validation ");
		}
	}

	@Test
	public void testAmbiguousFolderModel() throws InterceptorException
	{
		final MediaFolderModel model = new MediaFolderModel();
		model.setQualifier("fooFolder");

		when(mediaService.getFolder(eq("fooFolder")))
				.thenThrow(new AmbiguousIdentifierException("foo not found"));
		try
		{
			validator.onValidate(model, ctx);
			Assert.fail("Ambiguous folder qualifier should break validation ");
		}
		catch (final InterceptorException ie)
		{
			//
		}
	}

	@Test
	public void shouldThrowExceptionWhenAddingMediaFolderWithInvalidName()
	{
		//given
		final MediaFolderModel mediaFolder = getTestMediaFolderModel();
		when(storageStrategy.hasValidMediaFolderName(any())).thenReturn(false);

		//when, then
		assertThatThrownBy(() -> validator.onValidate(mediaFolder, ctx))
				.isInstanceOf(InterceptorException.class)
				.hasMessageContaining("Media folder with qualifier: " + FOLDER_QUALIFIER
						+ " doesn't complies with storage strategy naming convention");
	}

	@Test
	public void shouldNotThrowExceptionWhenAddingMediaFolderWithValidName()
	{
		//given
		final MediaFolderModel mediaFolder = getTestMediaFolderModel();
		when(mediaService.getFolder(FOLDER_QUALIFIER)).thenReturn(mediaFolder);
		boolean exception = false;

		//when
		try
		{
			validator.onValidate(mediaFolder, ctx);
		}
		catch (final Exception e)
		{
			exception = true;
		}

		//then
		assertThat(exception).isFalse();
	}

	@Test
	public void shouldValidateMediaFolderNameWhenValidationIsEnabled() throws InterceptorException
	{
		//given
		final MediaFolderModel mediaFolder = getTestMediaFolderModel();
		when(mediaService.getFolder(FOLDER_QUALIFIER)).thenReturn(mediaFolder);

		//when
		validator.onValidate(mediaFolder, ctx);

		//then
		verify(storageStrategy).hasValidMediaFolderName(any());
	}

	@Test
	public void shouldNotValidateMediaFolderNameWhenValidationIsDisabled() throws InterceptorException
	{
		//given
		final MediaFolderModel mediaFolder = getTestMediaFolderModel();
		when(mediaFolderConfig.isMediaFolderNameValidationEnabled()).thenReturn(false);
		when(mediaService.getFolder(FOLDER_QUALIFIER)).thenReturn(mediaFolder);

		//when
		validator.onValidate(mediaFolder, ctx);

		//then
		verify(storageStrategy, never()).hasValidMediaFolderName(any());
	}

	private MediaFolderModel getTestMediaFolderModel()
	{
		final MediaFolderModel mediaFolder = new MediaFolderModel();
		mediaFolder.setQualifier(FOLDER_QUALIFIER);

		return mediaFolder;
	}
}
