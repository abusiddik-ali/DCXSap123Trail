/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.validation.services.impl;

import static de.hybris.platform.servicelayer.ServicelayerTest.createCoreData;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.model.c2l.LanguageModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.ServicelayerTest;
import de.hybris.platform.servicelayer.ServicelayerTestLogic;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.validation.enums.Severity;
import de.hybris.platform.validation.exceptions.HybrisConstraintViolation;
import de.hybris.platform.validation.model.constraints.NotEmptyConstraintModel;
import de.hybris.platform.validation.services.ValidationService;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class LocalizedValidationMessagesIntegrationTest extends ServicelayerTransactionalBaseTest
{

	@Resource
	private ValidationService validationService;

	@Resource
	private ModelService modelService;

	@Resource
	private CommonI18NService commonI18NService;

	@Resource
	private I18NService i18NService;

	@Before
	public void prepare() throws Exception
	{
		createCoreData();
	}

	@After
	public void cleanUpValidationEngine()
	{
		validationService.reloadValidationEngine();
	}

	@Test
	public void testConstraintForEnglishLocale()
	{
		//given
		i18NService.setCurrentLocale(Locale.ENGLISH);
		final ProductModel productToValidate = prepareTestData();

		//when
		final Set<HybrisConstraintViolation> hybrisConstraintViolations = validationService.validate(productToValidate);

		//then
		assertThat(hybrisConstraintViolations).hasSize(4);
		assertThat(
				hybrisConstraintViolations.stream().map(violation -> violation.getLocalizedMessage())).containsExactlyInAnyOrder(
				"Product cannot have empty attribute 'name' in language: English.",
				"Product cannot have empty attribute 'name' in language: German.",
				"Product cannot have empty attribute 'description' in language: German.",
				"Product cannot have empty attribute 'description' in language: English.");
	}

	@Test
	public void testConstraintForGermanLocale()
	{
		//given
		i18NService.setCurrentLocale(Locale.GERMAN);
		final ProductModel productToValidate = prepareTestData();

		//when
		final Set<HybrisConstraintViolation> hybrisConstraintViolations = validationService.validate(productToValidate);

		//then
		assertThat(hybrisConstraintViolations).hasSize(4);
		assertThat(
				hybrisConstraintViolations.stream().map(violation -> violation.getLocalizedMessage())).containsExactlyInAnyOrder(
				"Product darf nicht ein leeres Attribut 'name' haben in Sprache: Englisch.",
				"Product darf nicht ein leeres Attribut 'name' haben in Sprache: Deutsch.",
				"Product darf nicht ein leeres Attribut 'description' haben in Sprache: Deutsch.",
				"Product darf nicht ein leeres Attribut 'description' haben in Sprache: Englisch.");

	}

	private ProductModel prepareTestData()
	{
		final NotEmptyConstraintModel constraintName = modelService.create(NotEmptyConstraintModel.class);
		constraintName.setId("testConstraint");
		constraintName.setSeverity(Severity.WARN);
		constraintName.setActive(true);
		constraintName.setTarget(ProductModel.class);
		constraintName.setQualifier("name");
		constraintName.setMessage("Product cannot have empty attribute 'name'", Locale.ENGLISH);
		constraintName.setMessage("Product darf nicht ein leeres Attribut 'name' haben", Locale.GERMAN);
		final HashSet<LanguageModel> languageModels = new HashSet<>(commonI18NService.getAllLanguages());
		constraintName.setLanguages(languageModels);
		modelService.save(constraintName);


		final NotEmptyConstraintModel constraintDesc = modelService.create(NotEmptyConstraintModel.class);
		constraintDesc.setId("testConstraint2");
		constraintDesc.setSeverity(Severity.WARN);
		constraintDesc.setActive(true);
		constraintDesc.setTarget(ProductModel.class);
		constraintDesc.setQualifier("description");
		constraintDesc.setMessage("Product cannot have empty attribute 'description'", Locale.ENGLISH);
		constraintDesc.setMessage("Product darf nicht ein leeres Attribut 'description' haben", Locale.GERMAN);
		constraintDesc.setLanguages(languageModels);
		modelService.save(constraintDesc);

		validationService.reloadValidationEngine();

		final CatalogModel catalogModel = modelService.create(CatalogModel.class);
		catalogModel.setId("testCatalog");
		modelService.save(catalogModel);

		final CatalogVersionModel catalogVersionModel = modelService.create(CatalogVersionModel.class);
		catalogVersionModel.setCatalog(catalogModel);
		catalogVersionModel.setVersion("test");
		modelService.save(catalogVersionModel);

		final ProductModel productToValidate = modelService.create(ProductModel.class);
		productToValidate.setCatalogVersion(catalogVersionModel);
		productToValidate.setCode("testCode");
		modelService.save(productToValidate);
		return productToValidate;
	}
}
