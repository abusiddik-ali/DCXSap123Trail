/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.catalog.enums.ClassificationAttributeTypeEnum;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.catalog.model.ProductFeatureModel;
import de.hybris.platform.catalog.model.classification.ClassAttributeAssignmentModel;
import de.hybris.platform.catalog.model.classification.ClassificationAttributeModel;
import de.hybris.platform.catalog.model.classification.ClassificationAttributeValueModel;
import de.hybris.platform.catalog.model.classification.ClassificationClassModel;
import de.hybris.platform.catalog.model.classification.ClassificationSystemModel;
import de.hybris.platform.catalog.model.classification.ClassificationSystemVersionModel;
import de.hybris.platform.catalog.synchronization.CatalogSynchronizationService;
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

public class MultivaluedProductFeatureOrderTest extends ServicelayerBaseTest
{
	@Resource
	protected CatalogSynchronizationService catalogSynchronizationService;
	@Resource
	private ModelService modelService;
	@Resource
	private I18NService i18NService;
	@Resource
	private ProductService productService;

	private final String productCode = "productA";

	private ProductModel stagedProduct;

	private ProductFeatureModel productFeatureA;
	private ProductFeatureModel productFeatureB;
	private ProductFeatureModel productFeatureC;

	private CatalogVersionModel catalogVersionStaged;

	private CatalogVersionModel catalogVersionOnline;

	private ClassificationClassModel ccm;

	private ClassificationAttributeValueModel classificationAttributeValueA;
	private ClassificationAttributeValueModel classificationAttributeValueB;
	private ClassificationAttributeValueModel classificationAttributeValueC;
	private ClassificationSystemVersionModel classificationSystemVersionStaged;

	private ClassificationAttributeModel classificationAttribute;
	private ClassAttributeAssignmentModel classAttributeAssignment;

	@Before
	public void prepare() throws Exception
	{
		ServicelayerTestLogic.createCoreData();
	}

	@Test
	public void testCorrectOrder()
	{
		createCatalogAndCatalogVersions();
		createClassificationClass();
		createProduct();
		createClassificationAttributeValues();
		createClassAttributeAssignment();
		createProductFeatures();

		synchronize();
		assertProductFeatures();

		rearrange();
		synchronize();
		assertProductFeaturesAfterRearrangement();
	}

	private void assertProductFeaturesAfterRearrangement()
	{
		final ProductModel onlineProduct = productService.getProductForCode(catalogVersionOnline, productCode);

		assertThat(stagedProduct.getFeatures()).hasSize(3);
		assertThat(onlineProduct.getFeatures()).hasSize(3);

		assertThat(stagedProduct.getFeatures().get(0).getValuePosition()).isEqualTo(0);
		assertThat(stagedProduct.getFeatures().get(1).getValuePosition()).isEqualTo(2);
		assertThat(stagedProduct.getFeatures().get(2).getValuePosition()).isEqualTo(1);
		assertThat(onlineProduct.getFeatures().get(0).getValuePosition()).isEqualTo(0);
		assertThat(onlineProduct.getFeatures().get(1).getValuePosition()).isEqualTo(2);
		assertThat(onlineProduct.getFeatures().get(2).getValuePosition()).isEqualTo(1);


		assertThat(stagedProduct.getFeatures().get(0).getValue()).isEqualTo(classificationAttributeValueA);
		assertThat(stagedProduct.getFeatures().get(1).getValue()).isEqualTo(classificationAttributeValueB);
		assertThat(stagedProduct.getFeatures().get(2).getValue()).isEqualTo(classificationAttributeValueC);
		assertThat(onlineProduct.getFeatures().get(0).getValue()).isEqualTo(classificationAttributeValueA);
		assertThat(onlineProduct.getFeatures().get(1).getValue()).isEqualTo(classificationAttributeValueB);
		assertThat(onlineProduct.getFeatures().get(2).getValue()).isEqualTo(classificationAttributeValueC);
	}

	private void assertProductFeatures()
	{
		ProductModel onlineProduct = productService.getProductForCode(catalogVersionOnline, productCode);

		assertThat(stagedProduct.getFeatures()).hasSize(2);
		assertThat(onlineProduct.getFeatures()).hasSize(2);

		assertThat(stagedProduct.getFeatures().get(0).getValuePosition()).isEqualTo(0);
		assertThat(stagedProduct.getFeatures().get(1).getValuePosition()).isEqualTo(1);
		assertThat(onlineProduct.getFeatures().get(0).getValuePosition()).isEqualTo(0);
		assertThat(onlineProduct.getFeatures().get(1).getValuePosition()).isEqualTo(1);

		assertThat(stagedProduct.getFeatures().get(0).getValue()).isEqualTo(classificationAttributeValueA);
		assertThat(stagedProduct.getFeatures().get(1).getValue()).isEqualTo(classificationAttributeValueB);
		assertThat(onlineProduct.getFeatures().get(0).getValue()).isEqualTo(classificationAttributeValueA);
		assertThat(onlineProduct.getFeatures().get(1).getValue()).isEqualTo(classificationAttributeValueB);
	}

	private void createClassAttributeAssignment()
	{
		classificationAttribute = modelService.create(ClassificationAttributeModel.class);
		classificationAttribute.setCode("classificationAttribute1");
		classificationAttribute.setSystemVersion(classificationSystemVersionStaged);
		classificationAttribute.setDefaultAttributeValues(
				List.of(classificationAttributeValueA, classificationAttributeValueB, classificationAttributeValueC));
		modelService.saveAll();


		classAttributeAssignment = modelService.create(ClassAttributeAssignmentModel.class);
		classAttributeAssignment.setClassificationAttribute(classificationAttribute);
		classAttributeAssignment.setClassificationClass(ccm);
		classAttributeAssignment.setAttributeType(ClassificationAttributeTypeEnum.STRING);
		classAttributeAssignment.setMultiValued(true);
		modelService.saveAll();
	}

	private void createClassificationClass()
	{
		final ClassificationSystemModel classificationSystem = modelService.create(ClassificationSystemModel.class);
		classificationSystem.setId("classificationSystem1");

		classificationSystemVersionStaged = modelService.create(
				ClassificationSystemVersionModel.class);
		classificationSystemVersionStaged.setCatalog(classificationSystem);
		classificationSystemVersionStaged.setVersion("staged");

		final ClassificationSystemVersionModel classificationSystemVersionOnline = modelService.create(
				ClassificationSystemVersionModel.class);
		classificationSystemVersionOnline.setCatalog(classificationSystem);
		classificationSystemVersionOnline.setVersion("online");

		ccm = modelService.create(ClassificationClassModel.class);
		ccm.setCatalogVersion(classificationSystemVersionStaged);
		ccm.setCode("apparel_Classification");
	}

	private void createProduct()
	{
		stagedProduct = modelService.create(ProductModel.class);
		stagedProduct.setCatalogVersion(catalogVersionStaged);
		stagedProduct.setSupercategories(List.of(ccm));
		stagedProduct.setCode(productCode);
	}

	private void createCatalogAndCatalogVersions()
	{
		final CatalogModel cm = modelService.create(CatalogModel.class);
		cm.setId("catalog1");

		catalogVersionStaged = modelService.create(CatalogVersionModel.class);
		catalogVersionStaged.setCatalog(cm);
		catalogVersionStaged.setVersion("staged");
		catalogVersionStaged.setLanguages(List.of(i18NService.getLanguage("DE"), i18NService.getLanguage("EN")));

		catalogVersionOnline = modelService.create(CatalogVersionModel.class);
		catalogVersionOnline.setCatalog(cm);
		catalogVersionOnline.setVersion("online");
		catalogVersionOnline.setLanguages(List.of(i18NService.getLanguage("DE"), i18NService.getLanguage("EN")));

		modelService.saveAll();
	}

	private void createClassificationAttributeValues()
	{
		classificationAttributeValueA = createClassificationAttributeValue(
				classificationSystemVersionStaged, "hasLining_1");

		classificationAttributeValueB = createClassificationAttributeValue(
				classificationSystemVersionStaged, "hasLining_2");

		classificationAttributeValueC = createClassificationAttributeValue(
				classificationSystemVersionStaged, "hasLining_3");

		modelService.saveAll();
	}

	private void createProductFeatures()
	{
		productFeatureA = modelService.create(ProductFeatureModel.class);
		productFeatureA.setClassificationAttributeAssignment(classAttributeAssignment);
		productFeatureA.setValue(classificationAttributeValueA);
		productFeatureA.setProduct(stagedProduct);
		productFeatureA.setValuePosition(0);
		modelService.save(productFeatureA);

		productFeatureB = modelService.create(ProductFeatureModel.class);
		productFeatureB.setClassificationAttributeAssignment(classAttributeAssignment);
		productFeatureB.setValue(classificationAttributeValueB);
		productFeatureB.setProduct(stagedProduct);
		productFeatureB.setValuePosition(1);

		modelService.save(productFeatureB);
	}

	private void rearrange()
	{
		productFeatureB.setValuePosition(2);

		modelService.save(productFeatureB);

		productFeatureC = modelService.create(ProductFeatureModel.class);
		productFeatureC.setClassificationAttributeAssignment(classAttributeAssignment);
		productFeatureC.setValue(classificationAttributeValueC);
		productFeatureC.setProduct(stagedProduct);
		productFeatureC.setValuePosition(1);

		modelService.save(productFeatureC);
	}


	private ClassificationAttributeValueModel createClassificationAttributeValue(
			final ClassificationSystemVersionModel classificationSystemVersionStaged, String code)
	{
		final ClassificationAttributeValueModel classificationAttributeValue1 = modelService.create(
				ClassificationAttributeValueModel.class);
		classificationAttributeValue1.setCode(code);
		classificationAttributeValue1.setSystemVersion(classificationSystemVersionStaged);
		return classificationAttributeValue1;
	}

	private void synchronize()
	{
		catalogSynchronizationService.synchronizeFully(catalogVersionStaged, catalogVersionOnline);
	}
} 