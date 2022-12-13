/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.catalog.synchronization.CatalogSynchronizationService;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.ServicelayerTestLogic;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.List;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;

public class ProductOrderSynchronizationTest extends ServicelayerBaseTest
{
	@Resource
	protected CatalogSynchronizationService catalogSynchronizationService;
	@Resource
	private ModelService modelService;
	@Resource
	private I18NService i18NService;
	@Resource
	private ProductService productService;

	private final String productCodeA = "productA";
	private final String productCodeB = "productB";
	private final String productCodeC = "productC";

	private ProductModel stagedProductA;
	private ProductModel stagedProductB;
	private ProductModel stagedProductC;

	private PK onlineProductAPK;
	private PK onlineProductBPK;

	private CatalogVersionModel catalogVersionStaged;

	private CatalogVersionModel catalogVersionOnline;

	@Before
	public void prepare() throws Exception
	{
		ServicelayerTestLogic.createCoreData();
	}

	@Test
	public void testCorrectOrder()
	{
		createCatalogAndCatalogVersions();
		createProducts();
		synchronize();
		assertProducts();

		rearrange();
		synchronize();
		assertProductsAfterRearrangement();
	}

	private void rearrange()
	{
		stagedProductB.setCode("temp");
		modelService.save(stagedProductB);

		stagedProductC = modelService.create(ProductModel.class);
		stagedProductC.setCatalogVersion(catalogVersionStaged);
		stagedProductC.setCode(productCodeB);
		modelService.save(stagedProductC);

		stagedProductB.setCode(productCodeC);
		modelService.save(stagedProductB);
	}

	private void assertProducts()
	{
		final ProductModel productByCVA = productService.getProductForCode(catalogVersionOnline, productCodeA);
		assertThat(productByCVA).isNotNull();
		onlineProductAPK = productByCVA.getPk();

		final ProductModel productByCVB = productService.getProductForCode(catalogVersionOnline, productCodeB);
		assertThat(productByCVB).isNotNull();
		onlineProductBPK = productByCVB.getPk();
	}

	private void assertProductsAfterRearrangement()
	{
		final ProductModel productByCVA = productService.getProductForCode(catalogVersionOnline, productCodeA);
		assertThat(productByCVA).isNotNull();
		assertThat(productByCVA.getPk()).isEqualTo(onlineProductAPK);

		final ProductModel productByCVB = productService.getProductForCode(catalogVersionOnline, productCodeC);
		assertThat(productByCVB).isNotNull();
		assertThat(productByCVB.getPk()).isEqualTo(onlineProductBPK);

		final ProductModel productByCVC = productService.getProductForCode(catalogVersionOnline, productCodeB);
		assertThat(productByCVC).isNotNull();
	}

	private void createProducts()
	{
		stagedProductA = modelService.create(ProductModel.class);
		stagedProductA.setCatalogVersion(catalogVersionStaged);
		stagedProductA.setCode(productCodeA);
		modelService.saveAll();

		stagedProductB = modelService.create(ProductModel.class);
		stagedProductB.setCatalogVersion(catalogVersionStaged);
		stagedProductB.setCode(productCodeB);
		modelService.saveAll();
	}

	private void createCatalogAndCatalogVersions()
	{
		final CatalogModel cm1 = modelService.create(CatalogModel.class);
		cm1.setId("catalog1");

		catalogVersionStaged = modelService.create(CatalogVersionModel.class);
		catalogVersionStaged.setCatalog(cm1);
		catalogVersionStaged.setVersion("staged");
		catalogVersionStaged.setLanguages(List.of(i18NService.getLanguage("EN")));

		catalogVersionOnline = modelService.create(CatalogVersionModel.class);
		catalogVersionOnline.setCatalog(cm1);
		catalogVersionOnline.setVersion("online");
		catalogVersionOnline.setLanguages(List.of(i18NService.getLanguage("EN")));

		modelService.saveAll();
	}

	private void synchronize()
	{
		catalogSynchronizationService.synchronizeFully(catalogVersionStaged, catalogVersionOnline);
	}
}