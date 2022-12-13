/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.jalo.synchronization;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.jalo.synchronization.SynchronizationTestHelper.Builder;
import de.hybris.platform.catalog.jalo.synchronization.SynchronizationTestHelper.ConfigureSyncCronJob;
import de.hybris.platform.catalog.jalo.synchronization.SynchronizationTestHelper.SyncOperation;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commons.model.DocumentModel;
import de.hybris.platform.commons.model.FormatterModel;
import de.hybris.platform.commons.model.XMLTransformFormatterModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.security.PrincipalModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.persistence.audit.gateway.AuditRecord;
import de.hybris.platform.servicelayer.ServicelayerTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import javax.annotation.Resource;

import de.hybris.platform.servicelayer.type.TypeService;
import de.hybris.platform.variants.model.VariantProductModel;
import de.hybris.platform.variants.model.VariantTypeModel;

import org.apache.commons.collections.bag.SynchronizedSortedBag;
import org.assertj.core.api.Assertions;
import org.junit.Test;


@IntegrationTest
public class SynchronizationMediaTest extends ServicelayerTest
{
	private static final String CATALOG = "testCatalog";
	private static final String SRC_CATALOG_VERSION = "srcCatalog";
	private static final String DST_CATALOG_VERSION = "dstCatalog";
	private static final String CATEGORY = "category";
	private static final String SUB_CATEGORY = "subcategory";

	@Resource
	private ModelService modelService;

	@Resource
	private FlexibleSearchService flexibleSearchService;

	@Test
	public void shouldSynchronizeMediaModelsForAddOperation() throws Exception
	{
		givenTestCatalogWithVersions();

		assertThat(allMediasFor(srcCatalogVersion())).isNotNull().hasSize(2);

		performSynchronization(null);

		assertThat(allMediasFor(dstCatalogVersion())).isNotNull().hasSize(2);
	}

	@Test
	public void shouldSynchronizeMediaModelsForModifOperation() throws Exception
	{
		givenTestCatalogWithVersions();

		for (final MediaModel mm : allMediasFor(srcCatalogVersion()))
		{
			mm.setDescription("new_description");
			modelService.save(mm);
		}

		performSynchronization(null);

		final Collection<MediaModel> mmCollect = allMediasFor(dstCatalogVersion());
		assertThat(mmCollect).isNotNull().hasSize(2);
		Assertions.assertThat(mmCollect).extracting(this::description).containsExactly("new_description", "new_description");
	}

	private String description(final MediaModel record)
	{
		return (String) record.getDescription();
	}

	private void givenTestCatalogWithVersions()
	{
		final CatalogModel catalog = modelService.create(CatalogModel.class);
		catalog.setId(CATALOG);

		final CatalogVersionModel sourceVersion = modelService.create(CatalogVersionModel.class);
		sourceVersion.setCatalog(catalog);
		sourceVersion.setVersion(SRC_CATALOG_VERSION);

		final CatalogVersionModel targetVersion = modelService.create(CatalogVersionModel.class);
		targetVersion.setCatalog(catalog);
		targetVersion.setVersion(DST_CATALOG_VERSION);

		final CategoryModel category1 = modelService.create(CategoryModel.class);
		category1.setCatalogVersion(sourceVersion);
		category1.setCode(CATEGORY);

		final CategoryModel category1Child = modelService.create(CategoryModel.class);
		category1Child.setCatalogVersion(sourceVersion);
		category1Child.setCode(SUB_CATEGORY);
		category1Child.setSupercategories(Arrays.asList(category1));

		final MediaModel media1 = modelService.create(MediaModel.class);
		media1.setCode("thumbnail");
		media1.setCatalogVersion(sourceVersion);

		final XMLTransformFormatterModel xmlMedia = modelService.create(XMLTransformFormatterModel.class);
		xmlMedia.setCode("xml");
		xmlMedia.setCatalogVersion(sourceVersion);
		xmlMedia.setOutputMimeType("OutputMimeType");
		xmlMedia.setInputMimeType("InputMimeType");
		xmlMedia.setScript("SomeScript");

		category1.setMedias(Arrays.asList(media1, xmlMedia));
		category1.setCategories(Arrays.asList(category1Child));
		modelService.saveAll();
	}

	private void performSynchronization(final ConfigureSyncCronJob configure, final SyncOperation... operations)
	{
		final Builder builder = SynchronizationTestHelper.builder(srcCatalogVersion(), dstCatalogVersion());

		final SynchronizationTestHelper helper = builder.configure(configure).add(operations).build();

		helper.performSynchronization();
	}

	private CatalogModel testCatalog()
	{
		final CatalogModel example = new CatalogModel();
		example.setId(CATALOG);
		return flexibleSearchService.getModelByExample(example);
	}

	private CatalogVersionModel srcCatalogVersion()
	{
		final CatalogVersionModel example = new CatalogVersionModel();
		example.setCatalog(testCatalog());
		example.setVersion(SRC_CATALOG_VERSION);
		return flexibleSearchService.getModelByExample(example);
	}

	private CatalogVersionModel dstCatalogVersion()
	{
		final CatalogVersionModel example = new CatalogVersionModel();
		example.setCatalog(testCatalog());
		example.setVersion(DST_CATALOG_VERSION);
		return flexibleSearchService.getModelByExample(example);
	}

	private Collection<MediaModel> allMediasFor(final CatalogVersionModel catalogVersion)
	{
		final MediaModel example = new MediaModel();
		example.setCatalogVersion(catalogVersion);
		return flexibleSearchService.getModelsByExample(example);
	}

}
