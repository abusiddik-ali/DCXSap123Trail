/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog;

import static org.junit.Assert.fail;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.catalog.model.synchronization.CatalogVersionSyncScheduleMediaModel;
import de.hybris.platform.servicelayer.ServicelayerTransactionalTest;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.model.ModelService;

import javax.annotation.Resource;

import org.junit.Test;


@IntegrationTest
public class CatalogVersionSyncScheduleMediaTest extends ServicelayerTransactionalTest
{
	@Resource
	private ModelService modelService;


	@Test
	public void shouldThrowModelSavingExceptionWhenCronJobIsNull()
	{
		// given
		final CatalogVersionSyncScheduleMediaModel synchScheduleMedia = prepareCatalogVersionSynchScheduleMedia();
		synchScheduleMedia.setCronjob(null);

		try
		{
			// when
			modelService.save(synchScheduleMedia);
			fail("Should throw MediaSavingException e");
		}
		catch (final ModelSavingException e)
		{
			// then OK
		}
	}

	private CatalogVersionSyncScheduleMediaModel prepareCatalogVersionSynchScheduleMedia()
	{
		final CatalogVersionModel catalogVersion = prepareCatalogVersion();

		final CatalogVersionSyncScheduleMediaModel synchScheduleMedia = modelService.create("CatalogVersionSyncScheduleMedia");
		synchScheduleMedia.setCode("deo meo");
		synchScheduleMedia.setCatalogVersion(catalogVersion);

		return synchScheduleMedia;
	}

	private CatalogVersionModel prepareCatalogVersion()
	{
		final CatalogModel catalog = new CatalogModel();
		catalog.setId("cm1" + System.currentTimeMillis());

		final CatalogVersionModel catalogVersion = new CatalogVersionModel();
		catalogVersion.setCatalog(catalog);
		catalogVersion.setVersion("v1.0");

		modelService.saveAll(catalog, catalogVersion);

		return catalogVersion;
	}
}
