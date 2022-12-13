/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.synchronization;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.catalog.model.SyncItemJobModel;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.cronjob.model.LogFileModel;
import de.hybris.platform.persistence.hjmp.HybrisOptimisticLockingFailureException;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.model.ModelService;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.zip.ZipInputStream;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;


public class CatalogSynchronizationMultipleOptimisticLockingTest extends ServicelayerBaseTest
{
	public static final int NUM_OF_PRODUCTS = 200;
	public static final int NUM_OF_REFS = 20;
	public static final int NUMBER_OF_THREADS = 10;

	@Resource
	private ModelService modelService;
	@Resource(name = "catalogSynchronizationService")
	private CatalogSynchronizationService syncService;
	@Resource
	private MediaService mediaService;

	private CatalogVersionModel source, target;
	private List<ProductModel> sourceProducts;
	private CategoryModel category;

	@Before
	public void setUp() throws Exception
	{
		final CatalogModel catalog = createCatalog("TestCatalog");
		source = createCatalogVersion(catalog, "staged");
		target = createCatalogVersion(catalog, "online");

		category = createCategory(source, "c1");

		sourceProducts = createProducts(NUM_OF_PRODUCTS, source, category);
		category.setProducts(sourceProducts);
		modelService.save(category);
		createProductReferences(sourceProducts, NUM_OF_REFS);

		CatalogVersionAssert.assertThat(source).hasNumOfProducts(NUM_OF_PRODUCTS);
	}

	@Test
	public void shouldSynchronizeTwoCatalogsFullyMultithreaded() throws Exception
	{
		syncService.synchronizeFully(source, target, NUMBER_OF_THREADS);

		final ImmutableMap<String, String> expectedProperties = ImmutableMap.<String, String>builder().put("code", "MyCode")
		                                                                                              .put("ean", "MyEan")
		                                                                                              .build();
		CatalogVersionAssert.assertThat(target).hasNumOfProducts(NUM_OF_PRODUCTS);
		CatalogVersionAssert.assertThat(target).hasAllProductsWithPropertiesAs(expectedProperties);


		modifyProductsInSource();
		final ImmutableMap<String, String> expectedPropertiesAfterMod = ImmutableMap.<String, String>builder()
				.put("code", "NewCode").put("ean", "NewEan").build();
		CatalogVersionAssert.assertThat(source).hasNumOfProducts(NUM_OF_PRODUCTS);
		CatalogVersionAssert.assertThat(source).hasAllProductsWithPropertiesAs(expectedPropertiesAfterMod);

		syncService.synchronizeFully(source, target, NUMBER_OF_THREADS);
		CatalogVersionAssert.assertThat(target).hasNumOfProducts(NUM_OF_PRODUCTS);
		CatalogVersionAssert.assertThat(target).hasAllProductsWithPropertiesAs(expectedPropertiesAfterMod);
	}

	@Test
	public void shouldLogOptimisticLockingExceptionsToCronJobLogFile() throws Exception
	{
		syncService.synchronizeFully(source, target, NUMBER_OF_THREADS);
		final SyncItemJobModel jobModel = syncService.getSyncJob(source, target, null);

		final Collection<CronJobModel> cronJobs = jobModel.getCronJobs();
		assertThat(cronJobs).hasSize(1);

		final Collection<LogFileModel> logFiles = cronJobs.iterator().next().getLogFiles();
		assertThat(logFiles).hasSize(1);

		final byte[] rawZip = mediaService.getDataFromMedia(logFiles.iterator().next());
		assertThat(rawZip).isNotEmpty();

		try (final ZipInputStream zip = new ZipInputStream(new ByteArrayInputStream(rawZip)))
		{
			zip.getNextEntry();
			final String logFileData = new String(zip.readAllBytes(), StandardCharsets.UTF_8);
			assertThat(logFileData).contains(HybrisOptimisticLockingFailureException.class.getName());
		}
	}

	private void modifyProductsInSource()
	{
		int i = 0;
		for (final ProductModel product : sourceProducts)
		{
			product.setCode("NewCode" + i);
			product.setEan("NewEan" + i);
			modelService.save(product);
			i++;
		}
	}

	private CategoryModel createCategory(final CatalogVersionModel catalogVersionModel, final String code)
	{
		final CategoryModel cat = modelService.create(CategoryModel.class);
		cat.setCode(code);
		cat.setCatalogVersion(catalogVersionModel);
		return cat;
	}

	private CatalogModel createCatalog(final String id)
	{
		final CatalogModel catalog = modelService.create(CatalogModel.class);
		catalog.setId(id);
		modelService.save(catalog);

		return catalog;
	}

	private CatalogVersionModel createCatalogVersion(final CatalogModel catalog, final String version)
	{
		final CatalogVersionModel catalogVersion = modelService.create(CatalogVersionModel.class);
		catalogVersion.setVersion(version);
		catalogVersion.setCatalog(catalog);
		modelService.save(catalogVersion);

		return catalogVersion;
	}

	private List<ProductModel> createProducts(final int numOfProducts, final CatalogVersionModel version,
	                                          final CategoryModel category)
	{
		final List<ProductModel> products = new ArrayList<>(numOfProducts);
		for (int i = 0; i < numOfProducts; i++)
		{
			products.add(createProduct("MyCode-" + i, "MyEan-" + i, version, category));
		}

		return products;
	}

	private ProductModel createProduct(final String code, final String ean, final CatalogVersionModel version,
	                                   final CategoryModel category)
	{
		final ProductModel product = modelService.create(ProductModel.class);
		product.setCode(code);
		product.setEan(ean);
		product.setCatalogVersion(version);
		product.setSupercategories(Lists.newArrayList(category));
		modelService.save(product);

		return product;
	}

	private List<ProductReferenceModel> createProductReferences(final List<ProductModel> products, final int numOfRefs)
	{
		final List<ProductReferenceModel> refs = new ArrayList<>(numOfRefs);
		for (int i = 0; i < products.size(); i++)
		{
			final ProductModel sourceProduct = products.get(i);

			for (int j = 0; j < numOfRefs; j++)
			{
				final ProductModel targetProduct = products.get((i + j + 1) % products.size());

				refs.add(createProductReference("Ref" + i, sourceProduct, targetProduct));
			}
		}

		return refs;
	}

	private ProductReferenceModel createProductReference(final String qualifier, final ProductModel source,
	                                                     final ProductModel target)
	{
		final ProductReferenceModel ref = modelService.create(ProductReferenceModel.class);
		ref.setQualifier(qualifier);
		ref.setSource(source);
		ref.setTarget(target);
		ref.setQuantity(Integer.valueOf(1));
		ref.setActive(Boolean.FALSE);
		ref.setPreselected(Boolean.FALSE);

		modelService.save(ref);

		return ref;
	}

}
