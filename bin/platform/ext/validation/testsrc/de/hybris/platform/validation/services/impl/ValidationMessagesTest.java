/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.validation.services.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.model.c2l.LanguageModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.validation.enums.Severity;
import de.hybris.platform.validation.exceptions.HybrisConstraintViolation;
import de.hybris.platform.validation.model.constraints.NotEmptyConstraintModel;
import de.hybris.platform.validation.services.ValidationService;

import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class ValidationMessagesTest extends ServicelayerTransactionalBaseTest
{

	@Resource
	private ValidationService validationService;

	@Resource
	private ModelService modelService;

	@Resource
	private CommonI18NService commonI18NService;

	@Resource
	private I18NService i18NService;
	private Locale currentLocale;

	@Before
	public void setUp() throws Exception
	{
		currentLocale = i18NService.getCurrentLocale();
	}

	@After
	public void tearDown()
	{
		i18NService.setCurrentLocale(currentLocale);
		validationService.reloadValidationEngine();
	}

	@Test
	public void shouldReturnProperMessageOnContraintViolation()
	{
		i18NService.setCurrentLocale(Locale.ENGLISH);

		createNotEmptyConstraint("name");
		createNotEmptyConstraint("description");

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
		productToValidate.setName("Name", Locale.ENGLISH);
		modelService.save(productToValidate);

		final Set<HybrisConstraintViolation> hybrisConstraintViolations = validationService.validate(productToValidate);

		final LocalizedHybrisConstraintViolation localizableViolation = (LocalizedHybrisConstraintViolation) hybrisConstraintViolations
				.iterator().next();

		final String localizedMessage = localizableViolation.getLocalizedMessage();

		assertThat(hybrisConstraintViolations).hasSize(1);
		assertThat(localizedMessage).startsWith("Message for description");
	}

	private NotEmptyConstraintModel createNotEmptyConstraint(final String qualifier)
	{
		final NotEmptyConstraintModel constraint = modelService.create(NotEmptyConstraintModel.class);

		constraint.setId("testConstraint_" + qualifier);
		constraint.setSeverity(Severity.WARN);
		constraint.setActive(true);
		constraint.setTarget(ProductModel.class);
		constraint.setQualifier(qualifier);
		constraint.setMessage("Message for " + qualifier, Locale.ENGLISH);
		final HashSet<LanguageModel> languageModels = new HashSet<>(commonI18NService.getAllLanguages());
		constraint.setLanguages(languageModels);
		modelService.save(constraint);

		return constraint;
	}
}
