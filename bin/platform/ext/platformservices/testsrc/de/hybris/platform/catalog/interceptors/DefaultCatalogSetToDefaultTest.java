/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.interceptors;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.catalog.CatalogService;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.servicelayer.ServicelayerTransactionalTest;
import de.hybris.platform.servicelayer.model.ModelService;

import javax.annotation.Resource;

import org.junit.Test;


public class DefaultCatalogSetToDefaultTest extends ServicelayerTransactionalTest
{

	@Resource
	private ModelService modelService;
	@Resource
	private CatalogService catalogService;

	@Test
	public void shouldSetDefaultCatalogToDefault()
	{
		// given
		final CatalogModel catalogA = modelService.create(CatalogModel.class);
		catalogA.setId("CATALOG_A");
		catalogA.setName("CATALOG_A");
		catalogA.setDefaultCatalog(Boolean.TRUE);
		modelService.save(catalogA);

		// when
		final CatalogModel defaultCatalog = catalogService.getDefaultCatalog();
		// sanity check
		assertThat(defaultCatalog).isEqualTo(catalogA);
		defaultCatalog.setDefaultCatalog(true);
		modelService.save(defaultCatalog);

		// then
		final CatalogModel defaultCatalogAfterUpdate = catalogService.getDefaultCatalog();
		assertThat(defaultCatalogAfterUpdate).isNotNull().isEqualTo(defaultCatalog);
	}
}
