/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.interceptors;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.classification.ClassAttributeAssignmentModel;
import de.hybris.platform.catalog.model.classification.ClassificationAttributeModel;
import de.hybris.platform.catalog.model.classification.ClassificationClassModel;
import de.hybris.platform.catalog.model.classification.ClassificationSystemModel;
import de.hybris.platform.catalog.model.classification.ClassificationSystemVersionModel;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.List;

import javax.annotation.Resource;

import org.junit.Test;

@IntegrationTest
public class ClassificationClassRemoveInterceptorIntegrationTest extends ServicelayerTransactionalBaseTest
{
	@Resource
	ModelService modelService;

	@Test
	public void shouldNotRemoveParentClassificationFeaturesWhenRemovingClassificationCategoryChild()
	{
		// given
		final ClassificationSystemVersionModel systemVersion = createClassificationSystemVersion();

		final ClassificationClassModel categoryParent = createClassificationClass(systemVersion, "cat-parent");

		final ClassificationClassModel categoryChild = createClassificationClass(systemVersion, "cat-child", categoryParent);

		final ClassAttributeAssignmentModel classAssignmentOfParentCategory = createClassAttributeWithAssignment(systemVersion,
				categoryParent, "attr-cat-parent");

		final ClassAttributeAssignmentModel classAssignmentOfChildCategory = createClassAttributeWithAssignment(systemVersion,
				categoryChild,
				"attr-cat-child");

		modelService.saveAll();

		assertThat(categoryParent.getAllClassificationAttributeAssignments()).containsOnly(classAssignmentOfParentCategory);
		assertThat(categoryChild.getAllClassificationAttributeAssignments()).containsOnly(classAssignmentOfParentCategory,
				classAssignmentOfChildCategory);

		// when
		modelService.remove(categoryChild);
		modelService.refresh(categoryParent);

		assertThat(categoryParent.getCategories()).isNullOrEmpty();

		// then
		assertThat(categoryParent.getAllClassificationAttributeAssignments()).containsOnly(classAssignmentOfParentCategory);
		assertThat(modelService.isRemoved(classAssignmentOfChildCategory)).isTrue();
	}

	@Test
	public void shouldNotRemoveParentClassificationFeaturesWhenRemovingClassificationCategoryGrandChild()
	{
		// given
		final ClassificationSystemVersionModel systemVersion = createClassificationSystemVersion();

		final ClassificationClassModel categoryParent = createClassificationClass(systemVersion, "cat-parent");

		final ClassificationClassModel categoryChild = createClassificationClass(systemVersion, "cat-child", categoryParent);

		final ClassificationClassModel categoryGrandChild = createClassificationClass(systemVersion, "cat-grandchild",
				categoryChild);

		final ClassAttributeAssignmentModel classAssignmentOfParentCategory = createClassAttributeWithAssignment(systemVersion,
				categoryParent, "attr-cat-parent");

		final ClassAttributeAssignmentModel classAssignmentOfChildCategory = createClassAttributeWithAssignment(systemVersion,
				categoryChild,
				"attr-cat-child");

		final ClassAttributeAssignmentModel classAssignmentOfGrandChildCategory = createClassAttributeWithAssignment(systemVersion,
				categoryGrandChild,
				"attr-cat-grandchild");

		modelService.saveAll();

		assertThat(categoryParent.getAllClassificationAttributeAssignments()).containsOnly(classAssignmentOfParentCategory);
		assertThat(categoryChild.getAllClassificationAttributeAssignments()).containsOnly(classAssignmentOfParentCategory,
				classAssignmentOfChildCategory);
		assertThat(categoryGrandChild.getAllClassificationAttributeAssignments()).containsOnly(classAssignmentOfParentCategory,
				classAssignmentOfChildCategory, classAssignmentOfGrandChildCategory);

		// when
		modelService.remove(categoryGrandChild);
		modelService.refresh(categoryChild);
		modelService.refresh(categoryParent);

		assertThat(categoryChild.getCategories()).isNullOrEmpty();
		assertThat(categoryParent.getCategories()).containsOnly(categoryChild);

		// then
		assertThat(categoryParent.getAllClassificationAttributeAssignments()).containsOnly(classAssignmentOfParentCategory);
		assertThat(categoryChild.getAllClassificationAttributeAssignments()).containsOnly(classAssignmentOfParentCategory,
				classAssignmentOfChildCategory);
		assertThat(modelService.isRemoved(classAssignmentOfGrandChildCategory)).isTrue();
	}

	private ClassAttributeAssignmentModel createClassAttributeWithAssignment(final ClassificationSystemVersionModel systemVersion,
	                                                                     final ClassificationClassModel category,
	                                                                     final String attributeCode)
	{
		final ClassificationAttributeModel attributeOfCategory = modelService.create(ClassificationAttributeModel.class);
		attributeOfCategory.setCode(attributeCode);
		attributeOfCategory.setSystemVersion(systemVersion);

		final ClassAttributeAssignmentModel classAssigmentOfCategory = modelService.create(
				ClassAttributeAssignmentModel.class);
		classAssigmentOfCategory.setClassificationClass(category);
		classAssigmentOfCategory.setClassificationAttribute(attributeOfCategory);
		return classAssigmentOfCategory;
	}

	private ClassificationClassModel createClassificationClass(final ClassificationSystemVersionModel systemVersion,
	                                                           final String s,
	                                                           final CategoryModel... superCategories)
	{
		final ClassificationClassModel category = modelService.create(ClassificationClassModel.class);
		category.setCode(s);
		category.setCatalogVersion(systemVersion);
		category.setSupercategories(List.of(superCategories));
		return category;
	}

	private ClassificationSystemVersionModel createClassificationSystemVersion()
	{
		final ClassificationSystemModel system = modelService.create(ClassificationSystemModel.class);
		system.setId("cs1");

		final ClassificationSystemVersionModel systemVersion = modelService.create(ClassificationSystemVersionModel.class);
		systemVersion.setVersion("cs1-v1");
		systemVersion.setCatalog(system);
		return systemVersion;
	}
}