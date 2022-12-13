/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.servicelayer.interceptor.impl;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.media.MediaFolderModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.testframework.BulkPropertyConfigSwitcher;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockitoAnnotations;

@IntegrationTest
public class MediaFolderValidatorIntegrationTest extends ServicelayerBaseTest
{
	private MediaFolderValidator validator;
	private final BulkPropertyConfigSwitcher propertySwitcher = new BulkPropertyConfigSwitcher();
	private final String MEDIA_FOLDER_NAME_VALIDATION_DEFAULT_SETTING = "media.default.mediaFolderName.validation";
	private final String MEDIA_FOLDER_NAME_VALIDATION_GLOBAL_SETTING = "media.globalSettings.%s.mediaFolderName.validation";
	private final String MEDIA_FOLDER_NAME_VALIDATION_FOLDER_SETTING = "media.folder.%s.mediaFolderName.validation";
	private final String STORAGE_STRATEGY_FOLDER_SETTING = "media.folder.%s.storage.strategy";
	private final String WINDOWS_AZURE_STORAGE_STRATEGY = "windowsAzureBlobStorageStrategy";
	private final String AMAZON_MEDIA_STORAGE_STRATEGY = "s3MediaStorageStrategy";
	private final String LOCAL_FILE_STORAGE_STRATEGY = "localFileMediaStorageStrategy";
	private static final String FOLDER_QUALIFIER = "test";
	private static final String FOLDER_QUALIFIER_2 = "test2";
	private static final String FOLDER_QUALIFIER_3 = "test3";

	@Before
	public void setUp()
	{
		MockitoAnnotations.initMocks(this);
		validator = spy((MediaFolderValidator) Registry.getApplicationContext()
		                                               .getBean("MediaFolderValidator"));
	}

	@After
	public void tearDown()
	{
		propertySwitcher.switchAllBack();
	}

	@Test
	public void shouldValidateMediaFolderWhenValidationIsEnabledUsingDefaultSetting() throws InterceptorException
	{
		//given
		final MediaFolderModel mediaFolder = getTestMediaFolderModel(FOLDER_QUALIFIER);
		propertySwitcher.switchToValue(MEDIA_FOLDER_NAME_VALIDATION_DEFAULT_SETTING, "true");

		//when
		validator.onValidate(mediaFolder, null);

		//then
		verify(validator).hasValidMediaFolderName(any());
	}

	@Test
	public void shouldNotValidateMediaFolderWhenValidationIsDisabledUsingDefaultSetting() throws InterceptorException
	{
		//given
		final MediaFolderModel mediaFolder = getTestMediaFolderModel(FOLDER_QUALIFIER);
		propertySwitcher.switchToValue(MEDIA_FOLDER_NAME_VALIDATION_DEFAULT_SETTING, "false");

		//when
		validator.onValidate(mediaFolder, null);

		//then
		verify(validator, never()).hasValidMediaFolderName(any());
	}

	@Test
	public void shouldValidateMediaFolderWhenValidationIsEnabledUsingGlobalSetting() throws InterceptorException
	{
		//given
		final MediaFolderModel mediaFolder = getTestMediaFolderModel(FOLDER_QUALIFIER);
		propertySwitcher.switchToValue(String.format(MEDIA_FOLDER_NAME_VALIDATION_GLOBAL_SETTING, LOCAL_FILE_STORAGE_STRATEGY), "true");
		setStorageStrategyForGivenFolder(FOLDER_QUALIFIER, LOCAL_FILE_STORAGE_STRATEGY);

		//when
		validator.onValidate(mediaFolder, null);

		//then
		verify(validator).hasValidMediaFolderName(any());
	}

	@Test
	public void shouldNotValidateMediaFolderWhenValidationIsDisabledUsingGlobalSetting() throws InterceptorException
	{
		//given
		final MediaFolderModel mediaFolder = getTestMediaFolderModel(FOLDER_QUALIFIER);
		propertySwitcher.switchToValue(String.format(MEDIA_FOLDER_NAME_VALIDATION_GLOBAL_SETTING, LOCAL_FILE_STORAGE_STRATEGY), "false");
		setStorageStrategyForGivenFolder(FOLDER_QUALIFIER, LOCAL_FILE_STORAGE_STRATEGY);

		//when
		validator.onValidate(mediaFolder, null);

		//then
		verify(validator, never()).hasValidMediaFolderName(any());
	}

	@Test
	public void shouldValidateMediaFolderWhenValidationIsEnabledUsingFolderSetting() throws InterceptorException
	{
		//given
		final MediaFolderModel mediaFolder = getTestMediaFolderModel(FOLDER_QUALIFIER);
		setMediaFolderValidationForGivenFolder(FOLDER_QUALIFIER, "true");

		//when
		validator.onValidate(mediaFolder, null);

		//then
		verify(validator).hasValidMediaFolderName(any());
	}

	@Test
	public void shouldNotValidateMediaFolderWhenValidationIsDisabledUsingFolderSetting() throws InterceptorException
	{
		//given
		final MediaFolderModel mediaFolder = getTestMediaFolderModel(FOLDER_QUALIFIER);
		setMediaFolderValidationForGivenFolder(FOLDER_QUALIFIER, "false");

		//when
		validator.onValidate(mediaFolder, null);

		//then
		verify(validator, never()).hasValidMediaFolderName(any());
	}

	@Test
	public void shouldValidateOnlyMediaFolderConfiguredWithLocaleFileStorageStrategy() throws InterceptorException
	{
		//given
		final MediaFolderModel azureMediaFolder = getTestMediaFolderModel(FOLDER_QUALIFIER);
		final MediaFolderModel amazonMediaFolder = getTestMediaFolderModel(FOLDER_QUALIFIER_2);
		final MediaFolderModel localMediaFolder = getTestMediaFolderModel(FOLDER_QUALIFIER_3);

		propertySwitcher.switchToValue(MEDIA_FOLDER_NAME_VALIDATION_DEFAULT_SETTING, "false");
		propertySwitcher.switchToValue(String.format(MEDIA_FOLDER_NAME_VALIDATION_GLOBAL_SETTING, LOCAL_FILE_STORAGE_STRATEGY), "true");
		setStorageStrategyForGivenFolder(azureMediaFolder.getQualifier(), WINDOWS_AZURE_STORAGE_STRATEGY);
		setStorageStrategyForGivenFolder(amazonMediaFolder.getQualifier(), AMAZON_MEDIA_STORAGE_STRATEGY);
		setStorageStrategyForGivenFolder(localMediaFolder.getQualifier(), LOCAL_FILE_STORAGE_STRATEGY);

		//when, then
		validator.onValidate(azureMediaFolder, null);
		verify(validator, never()).hasValidMediaFolderName(any());
		reset(validator);

		validator.onValidate(amazonMediaFolder, null);
		verify(validator, never()).hasValidMediaFolderName(any());
		reset(validator);

		validator.onValidate(localMediaFolder, null);
		verify(validator).hasValidMediaFolderName(any());
	}

	@Test
	public void shouldValidateFoldersWithoutDisabledValidationWhenValidationIsEnabledUsingDefaultSetting() throws InterceptorException
	{
		//given
		final MediaFolderModel mediaFolder1 = getTestMediaFolderModel(FOLDER_QUALIFIER);
		final MediaFolderModel mediaFolder2 = getTestMediaFolderModel(FOLDER_QUALIFIER_2);
		final MediaFolderModel mediaFolder3 = getTestMediaFolderModel(FOLDER_QUALIFIER_3);

		propertySwitcher.switchToValue(MEDIA_FOLDER_NAME_VALIDATION_DEFAULT_SETTING, "true");
		setMediaFolderValidationForGivenFolder(mediaFolder3.getQualifier(), "false");

		//when, then
		validator.onValidate(mediaFolder1, null);
		verify(validator).hasValidMediaFolderName(any());
		reset(validator);

		validator.onValidate(mediaFolder2, null);
		verify(validator).hasValidMediaFolderName(any());
		reset(validator);

		validator.onValidate(mediaFolder3, null);
		verify(validator, never()).hasValidMediaFolderName(any());
	}

	@Test
	public void shouldValidateFoldersWithEnabledValidationWhenValidationIsDisabledUsingGlobalSetting() throws InterceptorException
	{
		//given
		final MediaFolderModel mediaFolder1 = getTestMediaFolderModel(FOLDER_QUALIFIER);
		final MediaFolderModel mediaFolder2 = getTestMediaFolderModel(FOLDER_QUALIFIER_2);
		final MediaFolderModel mediaFolder3 = getTestMediaFolderModel(FOLDER_QUALIFIER_3);

		propertySwitcher.switchToValue(String.format(MEDIA_FOLDER_NAME_VALIDATION_GLOBAL_SETTING, LOCAL_FILE_STORAGE_STRATEGY), "false");
		setStorageStrategyForGivenFolder(mediaFolder1.getQualifier(), LOCAL_FILE_STORAGE_STRATEGY);
		setStorageStrategyForGivenFolder(mediaFolder2.getQualifier(), LOCAL_FILE_STORAGE_STRATEGY);
		setStorageStrategyForGivenFolder(mediaFolder3.getQualifier(), LOCAL_FILE_STORAGE_STRATEGY);
		setMediaFolderValidationForGivenFolder(mediaFolder2.getQualifier(), "true");
		setMediaFolderValidationForGivenFolder(mediaFolder3.getQualifier(), "true");

		//when, then
		validator.onValidate(mediaFolder1, null);
		verify(validator, never()).hasValidMediaFolderName(any());
		reset(validator);

		validator.onValidate(mediaFolder2, null);
		verify(validator).hasValidMediaFolderName(any());
		reset(validator);

		validator.onValidate(mediaFolder3, null);
		verify(validator).hasValidMediaFolderName(any());
	}

	@Test
	public void shouldNotValidateFoldersWithDisabledValidationWhenValidationIsEnabledUsingDefaultAndGlobalSettings() throws InterceptorException
	{
		//given
		final MediaFolderModel mediaFolder1 = getTestMediaFolderModel(FOLDER_QUALIFIER);
		final MediaFolderModel mediaFolder2 = getTestMediaFolderModel(FOLDER_QUALIFIER_2);
		final MediaFolderModel mediaFolder3 = getTestMediaFolderModel(FOLDER_QUALIFIER_3);

		propertySwitcher.switchToValue(MEDIA_FOLDER_NAME_VALIDATION_DEFAULT_SETTING, "true");
		propertySwitcher.switchToValue(String.format(MEDIA_FOLDER_NAME_VALIDATION_GLOBAL_SETTING, LOCAL_FILE_STORAGE_STRATEGY), "true");
		setStorageStrategyForGivenFolder(mediaFolder1.getQualifier(), LOCAL_FILE_STORAGE_STRATEGY);
		setStorageStrategyForGivenFolder(mediaFolder2.getQualifier(), LOCAL_FILE_STORAGE_STRATEGY);
		setStorageStrategyForGivenFolder(mediaFolder3.getQualifier(), LOCAL_FILE_STORAGE_STRATEGY);
		setMediaFolderValidationForGivenFolder(mediaFolder1.getQualifier(), "false");
		setMediaFolderValidationForGivenFolder(mediaFolder2.getQualifier(), "false");
		setMediaFolderValidationForGivenFolder(mediaFolder3.getQualifier(), "false");

		//when, then
		validator.onValidate(mediaFolder1, null);
		verify(validator, never()).hasValidMediaFolderName(any());
		reset(validator);

		validator.onValidate(mediaFolder2, null);
		verify(validator, never()).hasValidMediaFolderName(any());
		reset(validator);

		validator.onValidate(mediaFolder3, null);
		verify(validator, never()).hasValidMediaFolderName(any());
		reset(validator);
	}

	@Test
	public void shouldNotValidateFoldersConfiguredWithDifferentStrategiesWhenValidationIsDisabledUsingDefaultSetting() throws InterceptorException
	{
		//given
		final MediaFolderModel mediaFolder1 = getTestMediaFolderModel(FOLDER_QUALIFIER);
		final MediaFolderModel mediaFolder2 = getTestMediaFolderModel(FOLDER_QUALIFIER_2);
		final MediaFolderModel mediaFolder3 = getTestMediaFolderModel(FOLDER_QUALIFIER_3);

		propertySwitcher.switchToValue(MEDIA_FOLDER_NAME_VALIDATION_DEFAULT_SETTING, "false");
		setStorageStrategyForGivenFolder(mediaFolder1.getQualifier(), LOCAL_FILE_STORAGE_STRATEGY);
		setStorageStrategyForGivenFolder(mediaFolder2.getQualifier(), WINDOWS_AZURE_STORAGE_STRATEGY);
		setStorageStrategyForGivenFolder(mediaFolder3.getQualifier(), AMAZON_MEDIA_STORAGE_STRATEGY);

		//when, then
		validator.onValidate(mediaFolder1, null);
		verify(validator, never()).hasValidMediaFolderName(any());
		reset(validator);

		validator.onValidate(mediaFolder2, null);
		verify(validator, never()).hasValidMediaFolderName(any());
		reset(validator);

		validator.onValidate(mediaFolder3, null);
		verify(validator, never()).hasValidMediaFolderName(any());
		reset(validator);
	}

	private void setStorageStrategyForGivenFolder(final String folderQualifier, final String storageStrategy)
	{
		propertySwitcher.switchToValue(String.format(STORAGE_STRATEGY_FOLDER_SETTING, folderQualifier), storageStrategy);
	}

	private void setMediaFolderValidationForGivenFolder(final String folderQualifier, final String validationEnabled)
	{
		propertySwitcher.switchToValue(String.format(MEDIA_FOLDER_NAME_VALIDATION_FOLDER_SETTING, folderQualifier), validationEnabled);
	}

	private MediaFolderModel getTestMediaFolderModel(final String folderQualifier)
	{
		final MediaFolderModel mediaFolder = new MediaFolderModel();
		mediaFolder.setQualifier(folderQualifier);

		return mediaFolder;
	}
}
