/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.jalo.classification;

import static junit.framework.Assert.assertTrue;
import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.catalog.model.classification.ClassificationAttributeModel;
import de.hybris.platform.catalog.model.classification.ClassificationAttributeValueModel;
import de.hybris.platform.catalog.model.classification.ClassificationSystemModel;
import de.hybris.platform.catalog.model.classification.ClassificationSystemVersionModel;
import de.hybris.platform.core.enums.TypeOfCollectionEnum;
import de.hybris.platform.core.model.type.AttributeDescriptorModel;
import de.hybris.platform.core.model.type.CollectionTypeModel;
import de.hybris.platform.servicelayer.ServicelayerTransactionalTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.type.TypeService;

import java.util.Collections;
import java.util.Set;

import javax.annotation.Resource;

import org.junit.Test;


public class ClassificationAttributeTest extends ServicelayerTransactionalTest
{

	@Resource
	private ModelService modelService;

	@Resource
	private TypeService typeService;

	private ClassificationSystemModel classificationSystem;
	private ClassificationSystemVersionModel classificationSystemVersion;
	private ClassificationAttributeModel classificationAttribute;
	private ClassificationAttributeValueModel classificationAttributeValue;

	private final long sleepingTime = 1000l;

	@Test
	public void testModifiedTime() throws Exception
	{
		classificationSystem = modelService.create(ClassificationSystemModel.class);
		classificationSystem.setId("testClassificationSystem");
		classificationSystemVersion = modelService.create(ClassificationSystemVersionModel.class);
		classificationSystemVersion.setCatalog(classificationSystem);
		classificationSystemVersion.setVersion("testVersion");
		classificationAttribute = modelService.create(ClassificationAttributeModel.class);
		classificationAttribute.setCode("testClassificationAttributeCode");
		classificationAttribute.setSystemVersion(classificationSystemVersion);
		classificationAttributeValue = modelService.create(ClassificationAttributeValueModel.class);
		classificationAttributeValue.setCode("classificationAttributeValueCode");
		classificationAttributeValue.setSystemVersion(classificationSystemVersion);
		modelService.saveAll();
		final long creationTimeBefore = classificationAttribute.getModifiedtime().getTime();

		Thread.sleep(sleepingTime);
		classificationAttribute.setDefaultAttributeValues(Collections.singletonList(classificationAttributeValue));
		modelService.save(classificationAttribute);
		final long modifiedTimeaAfter = classificationAttribute.getModifiedtime().getTime();

		final long difference = modifiedTimeaAfter - creationTimeBefore;
		assertTrue("modified time at least one second later", (difference - sleepingTime) >= 0);
	}

	@Test
	public void shouldTypeOfClassificationSystemVersionBeSet()
	{
		// given
		final ClassificationSystemModel classificationSystemModel = new ClassificationSystemModel();
		classificationSystemModel.setCatalogVersions(Set.of());

		// when
		final AttributeDescriptorModel attributeDescriptor = typeService.getAttributeDescriptor(
				ClassificationSystemModel._TYPECODE,
				ClassificationSystemModel.CATALOGVERSIONS);

		// then
		final CollectionTypeModel attributeType = (CollectionTypeModel) attributeDescriptor.getAttributeType();
		assertThat(classificationSystemModel.getCatalogVersions()).isInstanceOf(Set.class); // if the getter method returns a set
		assertThat(attributeType.getTypeOfCollection()).isEqualTo(
				TypeOfCollectionEnum.SET); // then we should expect the type of collection to be SET
	}

}
