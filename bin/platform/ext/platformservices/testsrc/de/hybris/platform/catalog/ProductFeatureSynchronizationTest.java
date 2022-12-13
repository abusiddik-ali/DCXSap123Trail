/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog;

import static de.hybris.platform.servicelayer.ServicelayerTest.createCoreData;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.catalog.jalo.synchronization.SynchronizationTestHelper;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.catalog.model.classification.ClassAttributeAssignmentModel;
import de.hybris.platform.catalog.model.classification.ClassificationAttributeModel;
import de.hybris.platform.catalog.model.classification.ClassificationClassModel;
import de.hybris.platform.catalog.model.classification.ClassificationSystemModel;
import de.hybris.platform.catalog.model.classification.ClassificationSystemVersionModel;
import de.hybris.platform.classification.ClassificationService;
import de.hybris.platform.classification.features.FeatureList;
import de.hybris.platform.classification.features.FeatureValue;
import de.hybris.platform.classification.features.UnlocalizedFeature;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.EmployeeModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.type.TypeService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.testframework.seed.ClassificationSystemTestDataCreator;

import java.util.Arrays;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;


public class ProductFeatureSynchronizationTest extends ServicelayerBaseTest
{
	@Resource
	private ModelService modelService;
	@Resource
	private UserService userService;
	@Resource
	private FlexibleSearchService flexibleSearchService;
	@Resource
	private ClassificationService classificationService;
	@Resource
	private TypeService typeService;
	private ClassificationClassModel classificationClass;
	private ClassificationAttributeModel classificationUserAttribute, classificationProductAttribute;
	private ClassificationSystemTestDataCreator creator;
	private CatalogModel catalog;
	private CatalogVersionModel catalogVersionStaged;
	private CatalogVersionModel catalogVersionOnline;
	private ClassAttributeAssignmentModel userAttributeAssignment, productAttributeAssignment;

	@Before
	public void setUp() throws Exception
	{
		createCoreData();

		creator = new ClassificationSystemTestDataCreator(modelService);

		catalog = creator.createCatalog();

		catalogVersionStaged = creator.createCatalogVersion("test-staged", catalog);
		catalogVersionOnline = creator.createCatalogVersion("test-online", catalog);

		final ClassificationSystemModel system = creator.createClassificationSystem("testClassificationSystem");
		final ClassificationSystemVersionModel systemVersion = creator.createClassificationSystemVersion("testVersion", system);
		classificationClass = creator.createClassificationClass("testClass", systemVersion);
		classificationUserAttribute = creator.createClassificationAttribute("userAttrubute", systemVersion);
		classificationProductAttribute = creator.createClassificationAttribute("productAttribute", systemVersion);

		userAttributeAssignment = creator.createRefClassAttributeAssignment(classificationUserAttribute, classificationClass,
				typeService.getComposedTypeForCode("User"));
		productAttributeAssignment = creator.createRefClassAttributeAssignment(classificationProductAttribute,
				classificationClass,
				typeService.getComposedTypeForCode("Product"));
	}

	@Test
	public void shouldSynchronizeAssignedRefererenceFratureToExistingOnlineVersionOfTheProduct()
	{
		// given
		final ProductModel product = creator.createProduct(catalogVersionStaged);

		final SynchronizationTestHelper synchronizationTestHelper = SynchronizationTestHelper
				.builder(catalogVersionStaged, catalogVersionOnline).build();
		assertThat(findProduct(product.getCode(), catalogVersionStaged)).isNotNull();
		assertThat(findProduct(product.getCode(), catalogVersionOnline)).isNull();
		assertThat(classificationService.getFeatures(product)).isEmpty();

		synchronizationTestHelper.performSynchronization();
		assertThat(findProduct(product.getCode(), catalogVersionStaged)).isNotNull();
		assertThat(findProduct(product.getCode(), catalogVersionOnline)).isNotNull();

		final EmployeeModel adminAsFeatureValue = userService.getAdminUser();
		classificationService.setFeature(product,
				new UnlocalizedFeature(userAttributeAssignment, new FeatureValue(adminAsFeatureValue)));
		// We need to add Product to ClassificationClass to make it work
		classificationClass.setProducts(Arrays.asList(product));
		modelService.save(classificationClass);

		// when
		synchronizationTestHelper.performSynchronization();
		final ProductModel syncedProduct = findProduct(product.getCode(), catalogVersionOnline);
		final FeatureList features = classificationService.getFeatures(syncedProduct);

		// then
		assertThat(syncedProduct).isNotNull();
		assertThat(features).isNotEmpty();
		assertThat(features.getFeatureByAssignment(userAttributeAssignment)).isNotNull();
		assertThat(features.getFeatureByAssignment(userAttributeAssignment).getValue().getValue()).isEqualTo(adminAsFeatureValue);
	}

	@Test
	public void shouldSynchronizeProductWithAssignedReferenceProductFeature()
	{
		// given
		final EmployeeModel adminAsFeatureValue = userService.getAdminUser();
		final ProductModel product = creator.createProduct(catalogVersionStaged);
		classificationService.setFeature(product,
				new UnlocalizedFeature(userAttributeAssignment, new FeatureValue(adminAsFeatureValue)));
		// We need to add Product to ClassificationClass to make it work
		classificationClass.setProducts(Arrays.asList(product));
		modelService.save(classificationClass);

		final SynchronizationTestHelper synchronizationTestHelper = SynchronizationTestHelper
				.builder(catalogVersionStaged, catalogVersionOnline).build();
		assertThat(findProduct(product.getCode(), catalogVersionStaged)).isNotNull();
		assertThat(findProduct(product.getCode(), catalogVersionOnline)).isNull();

		// when
		synchronizationTestHelper.performSynchronization();
		final ProductModel syncedProduct = findProduct(product.getCode(), catalogVersionOnline);
		final FeatureList features = classificationService.getFeatures(syncedProduct);


		// then
		assertThat(syncedProduct).isNotNull();
		assertThat(features).isNotEmpty();
		assertThat(features.getFeatureByAssignment(userAttributeAssignment)).isNotNull();
		assertThat(features.getFeatureByAssignment(userAttributeAssignment).getValue().getValue()).isEqualTo(adminAsFeatureValue);
	}

	@Test
	public void shouldSynchronizeProductWithAssignedProductFeatuerWhichHasAnotherProductAsValue()
	{
		// given
		final ProductModel product1 = creator.createProduct(catalogVersionStaged);
		final ProductModel product2 = creator.createProduct(catalogVersionStaged);
		classificationService.setFeature(product1,
				new UnlocalizedFeature(productAttributeAssignment, new FeatureValue(product2)));
		classificationClass.setProducts(Arrays.asList(product1));
		modelService.save(classificationClass);

		final SynchronizationTestHelper synchronizationTestHelper = SynchronizationTestHelper
				.builder(catalogVersionStaged, catalogVersionOnline).build();
		assertThat(findProduct(product1.getCode(), catalogVersionStaged)).isNotNull();
		assertThat(findProduct(product1.getCode(), catalogVersionOnline)).isNull();
		assertThat(findProduct(product2.getCode(), catalogVersionStaged)).isNotNull();
		assertThat(findProduct(product2.getCode(), catalogVersionOnline)).isNull();

		// when
		synchronizationTestHelper.performSynchronization();
		final ProductModel syncedProduct = findProduct(product1.getCode(), catalogVersionOnline);
		final FeatureList features = classificationService.getFeatures(syncedProduct);
		final ProductModel syncedProductFromFeature = findProduct(product2.getCode(), catalogVersionOnline);

		// then
		assertThat(syncedProduct).isNotNull();
		assertThat(syncedProductFromFeature).isNotNull();
		assertThat(features.getFeatureByAssignment(productAttributeAssignment)).isNotNull();
		assertThat(features.getFeatureByAssignment(productAttributeAssignment).getValue().getValue())
				.isEqualTo(syncedProductFromFeature);
		assertThat(((ProductModel) features.getFeatureByAssignment(productAttributeAssignment).getValue().getValue())
				.getCatalogVersion()).isEqualTo(catalogVersionOnline);
	}

	private ProductModel findProduct(final String code, final CatalogVersionModel catalogVersion)
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(
				"SELECT {PK} FROM {Product} WHERE {catalogVersion}=?ctgVer AND {code}=?code");
		fQuery.addQueryParameter("ctgVer", catalogVersion);
		fQuery.addQueryParameter("code", code);

		try
		{
			return flexibleSearchService.searchUnique(fQuery);
		}
		catch (final Exception e)
		{
			return null;
		}
	}
}
