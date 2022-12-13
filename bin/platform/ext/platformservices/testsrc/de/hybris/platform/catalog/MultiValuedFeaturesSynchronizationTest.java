/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.enums.ClassificationAttributeTypeEnum;
import de.hybris.platform.catalog.jalo.synchronization.SynchronizationTestHelper;
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
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.type.TypeService;

import java.util.Collections;
import java.util.List;

import javax.annotation.Resource;

import org.assertj.core.api.Assertions;
import org.junit.Test;


@IntegrationTest
public class MultiValuedFeaturesSynchronizationTest extends ServicelayerBaseTest
{
	@Resource
	private ModelService modelService;
	@Resource
	private TypeService typeService;
	@Resource
	private ProductService productService;
	@Resource
	private ClassificationService classificationService;
	@Resource
	private FlexibleSearchService flexibleSearchService;

	//HORST-5329
	@Test
	public void howToMakeSynchronizationVeryLongLongProcess()
	{
		final ClassificationSystemModel classificationSystemModel = modelService.create(ClassificationSystemModel._TYPECODE);
		classificationSystemModel.setId("classificationCatalog");
		modelService.save(classificationSystemModel);
		modelService.refresh(classificationSystemModel);

		final ClassificationSystemVersionModel classificationSystemVersionModelStaged = modelService
				.create(ClassificationSystemVersionModel._TYPECODE);
		classificationSystemVersionModelStaged.setCatalog(classificationSystemModel);
		classificationSystemVersionModelStaged.setVersion("staged");
		modelService.save(classificationSystemVersionModelStaged);
		modelService.refresh(classificationSystemVersionModelStaged);

		final ClassificationSystemVersionModel classificationSystemVersionModelOnline = modelService
				.create(ClassificationSystemVersionModel._TYPECODE);
		classificationSystemVersionModelOnline.setCatalog(classificationSystemModel);
		classificationSystemVersionModelOnline.setVersion("online");
		modelService.save(classificationSystemVersionModelOnline);
		modelService.refresh(classificationSystemVersionModelOnline);


		final ClassificationClassModel classificationClassModel = modelService.create(ClassificationClassModel._TYPECODE);
		classificationClassModel.setCatalogVersion(classificationSystemVersionModelStaged);
		classificationClassModel.setCode("classificationClass");
		modelService.save(classificationClassModel);
		modelService.refresh(classificationClassModel);


		final ClassificationAttributeModel classificationAttributeModel = modelService
				.create(ClassificationAttributeModel._TYPECODE);
		classificationAttributeModel.setCode("classificationAttribute");
		classificationAttributeModel.setSystemVersion(classificationSystemVersionModelStaged);
		modelService.save(classificationAttributeModel);
		modelService.refresh(classificationAttributeModel);


		final ClassAttributeAssignmentModel classAttributeAssignmentModel = modelService
				.create(ClassAttributeAssignmentModel._TYPECODE);
		classAttributeAssignmentModel.setAttributeType(ClassificationAttributeTypeEnum.REFERENCE);
		classAttributeAssignmentModel.setReferenceType(typeService.getComposedTypeForCode(MediaModel._TYPECODE));
		classAttributeAssignmentModel.setClassificationClass(classificationClassModel);
		classAttributeAssignmentModel.setClassificationAttribute(classificationAttributeModel);
		classAttributeAssignmentModel.setSystemVersion(classificationSystemVersionModelStaged);
		classAttributeAssignmentModel.setMultiValued(true);
		modelService.save(classAttributeAssignmentModel);
		modelService.refresh(classAttributeAssignmentModel);

		final MediaModel m1 = createMedia("m1", classificationSystemVersionModelStaged);
		final MediaModel m2 = createMedia("m2", classificationSystemVersionModelStaged);

		final ProductModel product = modelService.create(ProductModel._TYPECODE);
		product.setCode("product1");
		product.setCatalogVersion(classificationSystemVersionModelStaged);
		product.setSupercategories(Collections.singletonList(classificationClassModel));
		modelService.save(product);
		modelService.refresh(product);

		final UnlocalizedFeature feature = new UnlocalizedFeature(classAttributeAssignmentModel, new FeatureValue(m1),
				new FeatureValue(m2));
		classificationService.setFeature(product, feature);
		modelService.save(product);
		modelService.refresh(product);

		SynchronizationTestHelper.builder(classificationSystemVersionModelStaged, classificationSystemVersionModelOnline)
		                         .add(SynchronizationTestHelper.create(product))
		                         .add(SynchronizationTestHelper.create(classificationClassModel))
		                         .add(SynchronizationTestHelper.create(classificationAttributeModel))
		                         .add(SynchronizationTestHelper.create(m1))
		                         .add(SynchronizationTestHelper.create(m2))
		                         .build().performSynchronization(); // <-- synchronization will not finished

		final ProductModel syncProduct = productService.getProductForCode(classificationSystemVersionModelOnline, "product1");
		assertThat(syncProduct.getSupercategories()).hasSize(1);
		assertThat(syncProduct.getSupercategories().iterator().next().getCatalogVersion())
				.isEqualTo(classificationSystemVersionModelOnline);
		final FeatureList syncFeatures = classificationService.getFeatures(syncProduct);
		assertThat(syncFeatures).isNotNull();
		assertThat(syncFeatures.getFeatures()).hasSize(1);
		assertThat(syncFeatures.getFeatures().get(0)).isNotNull();
		final List<FeatureValue> vals = syncFeatures.getFeatures().get(0).getValues();
		Assertions.assertThat(vals).hasSize(2);
		Assertions.assertThat(vals.get(0).getValue()).isEqualTo(getMediaByExample("m1", classificationSystemVersionModelOnline));
		Assertions.assertThat(vals.get(1).getValue()).isEqualTo(getMediaByExample("m2", classificationSystemVersionModelOnline));
	}

	private MediaModel getMediaByExample(final String code, final CatalogVersionModel cv)
	{
		final MediaModel media = new MediaModel();
		media.setCatalogVersion(cv);
		media.setCode(code);
		return flexibleSearchService.getModelByExample(media);
	}

	private MediaModel createMedia(final String code, final CatalogVersionModel cv)
	{
		final MediaModel media = modelService.create(MediaModel._TYPECODE);
		media.setCode(code);
		media.setCatalogVersion(cv);
		modelService.save(media);
		modelService.refresh(media);
		return media;
	}
}
