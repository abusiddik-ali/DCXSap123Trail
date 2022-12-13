/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.enums.ClassificationAttributeTypeEnum;
import de.hybris.platform.catalog.jalo.ProductFeature;
import de.hybris.platform.catalog.model.ProductFeatureModel;
import de.hybris.platform.catalog.model.classification.ClassAttributeAssignmentModel;
import de.hybris.platform.catalog.model.classification.ClassificationAttributeModel;
import de.hybris.platform.catalog.model.classification.ClassificationClassModel;
import de.hybris.platform.catalog.model.classification.ClassificationSystemModel;
import de.hybris.platform.catalog.model.classification.ClassificationSystemVersionModel;
import de.hybris.platform.classification.ClassificationService;
import de.hybris.platform.classification.features.FeatureValue;
import de.hybris.platform.classification.features.UnlocalizedFeature;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.ServicelayerTransactionalTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.servicelayer.type.TypeService;
import org.junit.Test;

import javax.annotation.Resource;
import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;


@IntegrationTest
public class ProductFeaturesMultivaluedReferenceIntegrationTest extends ServicelayerTransactionalTest
{
	@Resource
	private ModelService modelService;
	@Resource
	private TypeService typeService;
	@Resource
	private ClassificationService classificationService;
	@Resource
	private FlexibleSearchService flexibleSearchService;

	private ClassificationSystemModel classificationSystemModel;
	private ClassificationClassModel classificationClassModel;
	private ClassAttributeAssignmentModel classAttributeAssignmentCollectionModel;
	private ClassificationSystemVersionModel classificationSystemVersionModelStaged;
	private ClassificationAttributeModel classificationAttributeCollectionModel;
	private ProductModel p1, p2, p3;

	@Test
	public void shouldSetTypeValueForMultivaluedReference()
	{
		//given
		prepareCatalogAndCategory();
		prepareProducts();

		//when
		final String query = "SELECT {pf.PK} FROM {ProductFeature as pf} WHERE {pf.product} = ?product";
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(query);
		fQuery.setResultClassList(Collections.singletonList(ProductFeatureModel.class));
		fQuery.addQueryParameter("product", p1);

		final SearchResult<ProductFeatureModel> result = flexibleSearchService.search(fQuery);
		final List<ProductFeatureModel> features = result.getResult();

		//then
		features.forEach(f -> {
			final Integer valueType = f.getProperty(ProductFeatureModel.VALUETYPE);
			assertThat(valueType).isNotNull();
			assertThat(valueType).isEqualTo(ProductFeature.TYPE_VALUE);
		});
	}

	private void prepareCatalogAndCategory()
	{
		classificationSystemModel = modelService.create(ClassificationSystemModel._TYPECODE);
		classificationSystemModel.setId("classificationCatalog");
		modelService.save(classificationSystemModel);
		modelService.refresh(classificationSystemModel);

		//version staged
		classificationSystemVersionModelStaged = modelService.create(ClassificationSystemVersionModel._TYPECODE);
		classificationSystemVersionModelStaged.setCatalog(classificationSystemModel);
		classificationSystemVersionModelStaged.setVersion("staged");
		modelService.save(classificationSystemVersionModelStaged);
		modelService.refresh(classificationSystemVersionModelStaged);


		classificationClassModel = modelService.create(ClassificationClassModel._TYPECODE);
		classificationClassModel.setCatalogVersion(classificationSystemVersionModelStaged);
		classificationClassModel.setCode("classificationClass");
		modelService.save(classificationClassModel);
		modelService.refresh(classificationClassModel);

		//attribute collection of reference
		classificationAttributeCollectionModel = modelService.create(ClassificationAttributeModel._TYPECODE);
		classificationAttributeCollectionModel.setCode("classificationAttributeCollectionAttribute");
		classificationAttributeCollectionModel.setSystemVersion(classificationSystemVersionModelStaged);
		modelService.save(classificationAttributeCollectionModel);
		modelService.refresh(classificationAttributeCollectionModel);

		//assign attribute collection of reference to category
		classAttributeAssignmentCollectionModel = modelService.create(ClassAttributeAssignmentModel._TYPECODE);
		classAttributeAssignmentCollectionModel.setAttributeType(ClassificationAttributeTypeEnum.REFERENCE);
		classAttributeAssignmentCollectionModel.setReferenceType(typeService.getComposedTypeForCode(ProductModel._TYPECODE));
		classAttributeAssignmentCollectionModel.setMultiValued(true);
		classAttributeAssignmentCollectionModel.setClassificationClass(classificationClassModel);
		classAttributeAssignmentCollectionModel.setClassificationAttribute(classificationAttributeCollectionModel);
		classAttributeAssignmentCollectionModel.setSystemVersion(classificationSystemVersionModelStaged);
		modelService.save(classAttributeAssignmentCollectionModel);
		modelService.refresh(classAttributeAssignmentCollectionModel);
	}

	private void prepareProducts()
	{
		p1 = createProduct("p1");
		p2 = createProduct("p2");
		p3 = createProduct("p3");

		p1.setSupercategories(Collections.singleton(classificationClassModel));
		modelService.save(p1);

		final UnlocalizedFeature feature = new UnlocalizedFeature(classAttributeAssignmentCollectionModel, new FeatureValue(p2),
				new FeatureValue(p3));
		classificationService.setFeature(p1, feature);
	}

	private ProductModel createProduct(final String code)
	{
		final ProductModel prod = modelService.create(ProductModel._TYPECODE);
		prod.setCode(code);
		prod.setCatalogVersion(classificationSystemVersionModelStaged);

		modelService.save(prod);
		modelService.refresh(prod);
		return prod;
	}

}
