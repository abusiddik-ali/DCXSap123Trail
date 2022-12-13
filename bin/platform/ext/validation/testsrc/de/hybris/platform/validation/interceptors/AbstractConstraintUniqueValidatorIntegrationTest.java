/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.validation.interceptors;

import static org.apache.commons.collections4.SetUtils.emptyIfNull;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.groups.Tuple.tuple;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.core.model.c2l.LanguageModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.type.AttributeDescriptorModel;
import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.jalo.user.Title;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.type.TypeService;
import de.hybris.platform.validation.annotations.XorNotNull;
import de.hybris.platform.validation.enums.Severity;
import de.hybris.platform.validation.exceptions.HybrisConstraintViolation;
import de.hybris.platform.validation.model.constraints.AttributeConstraintModel;
import de.hybris.platform.validation.model.constraints.TypeConstraintModel;
import de.hybris.platform.validation.model.constraints.XorNullReferenceConstraintModel;
import de.hybris.platform.validation.model.constraints.jsr303.NotNullConstraintModel;
import de.hybris.platform.validation.services.ConstraintService;
import de.hybris.platform.validation.services.ValidationService;
import de.hybris.platform.validation.services.impl.LocalizedHybrisConstraintViolation;

import java.util.Locale;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Supplier;

import javax.annotation.Resource;
import javax.validation.constraints.NotNull;

import org.assertj.core.api.Condition;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

@IntegrationTest
public class AbstractConstraintUniqueValidatorIntegrationTest extends ServicelayerBaseTest
{

	private final AbstractConstraintUniqueValidator validator = new AbstractConstraintUniqueValidator();
	@Resource
	CommonI18NService commonI18NService;
	@Resource
	private ConstraintService constraintService;
	@Resource
	private ModelService modelService;
	@Resource
	private TypeService typeService;
	@Resource
	private AttributeConstraintPreparer attributeConstraintPreparer;
	@Resource
	private TypeConstraintPreparer typeConstraintPreparer;
	@Resource
	private ValidationService validationService;

	private LanguageModel english;
	private LanguageModel german;


	@Before
	public void setUp() throws Exception
	{
		getOrCreateLanguage("en");
		getOrCreateLanguage("de");

		english = commonI18NService.getLanguage("en");
		german = commonI18NService.getLanguage("de");

		validator.setConstraintService(constraintService);
	}

	@After
	public void tearDown() throws Exception
	{
		validationService.reloadValidationEngine();
	}

	@Test
	public void shouldReturnTwoValidationErrorsWhenTwoConstraintsAreDefinedForDifferentLanguages()
	{
		final AttributeConstraintModel constraint1 = getDefaultLocalizedAttributeConstraintWithAttributeDescriptor();
		final AttributeConstraintModel constraint2 = getDefaultLocalizedAttributeConstraintWithAttributeDescriptor();

		constraint2.setLanguages(Set.of(english));
		constraint2.setMessage("constraint en c1");
		modelService.save(constraint2);

		constraint1.setMessage("constraint en,de c2");
		modelService.save(constraint1);

		validationService.reloadValidationEngine();

		final TitleModel title = modelService.create(TitleModel.class);

		title.setCode(UUID.randomUUID().toString());
		title.setName("nameDe", Locale.GERMAN);

		final Set<HybrisConstraintViolation> violations = validationService.validate(title);

		assertThat(violations).hasSize(2)
		                      .hasOnlyElementsOfType(LocalizedHybrisConstraintViolation.class)
		                      .extracting(HybrisConstraintViolation::getProperty,
				                      t -> ((LocalizedHybrisConstraintViolation) t).getViolationLanguage())
		                      .containsExactlyInAnyOrder(tuple(TitleModel.NAME, Locale.ENGLISH),
				                      tuple(TitleModel.NAME, Locale.ENGLISH));

		assertThat(violations).extracting(HybrisConstraintViolation::getLocalizedMessage)
		                      .areExactly(1, new Condition<>(s -> s.contains("constraint en c1"), ""))
		                      .areExactly(1, new Condition<>(s -> s.contains("constraint en,de c2"), ""));
	}

	@Test
	public void shouldReturnTwoValidationErrorsWhenTwoConstraintsAreDefinedForDifferentSeverity()
	{
		final AttributeConstraintModel constraint1 = getDefaultLocalizedAttributeConstraintWithAttributeDescriptor();
		final AttributeConstraintModel constraint2 = getDefaultLocalizedAttributeConstraintWithAttributeDescriptor();

		constraint1.setMessage("constraint ERROR c1");
		modelService.save(constraint1);

		constraint2.setSeverity(Severity.WARN);
		constraint2.setMessage("constraint WARN c2");
		modelService.save(constraint2);


		validationService.reloadValidationEngine();

		final TitleModel title = modelService.create(TitleModel.class);

		title.setCode(UUID.randomUUID().toString());
		title.setName("nameDe", Locale.GERMAN);

		final Set<HybrisConstraintViolation> violations = validationService.validate(title);

		assertThat(violations).hasSize(2)
		                      .hasOnlyElementsOfType(LocalizedHybrisConstraintViolation.class)
		                      .extracting(HybrisConstraintViolation::getProperty,
				                      HybrisConstraintViolation::getViolationSeverity)
		                      .containsExactlyInAnyOrder(tuple(TitleModel.NAME, Severity.WARN),
				                      tuple(TitleModel.NAME, Severity.ERROR));

		final HybrisConstraintViolation error = violations.stream()
		                                                  .filter(v -> v.getViolationSeverity() == Severity.ERROR)
		                                                  .findFirst().orElseThrow();
		assertThat(error.getLocalizedMessage()).contains("constraint ERROR c1");

		final HybrisConstraintViolation warn = violations.stream()
		                                                 .filter(v -> v.getViolationSeverity() == Severity.WARN)
		                                                 .findFirst().orElseThrow();
		assertThat(warn.getLocalizedMessage()).contains("constraint WARN c2");
	}

	@Test
	@Ignore("should it work???")
	public void shouldGenerateValidConstraintViolationForPOJOConstraint()
	{
		final AttributeConstraintModel constraint = getDefaultAttributeConstraintWithPOJO();

		final String randomString = UUID.randomUUID().toString();
		constraint.setMessage(randomString);

		modelService.save(constraint);

		validationService.reloadValidationEngine();
		final Set<HybrisConstraintViolation> violations = validationService.validate(new TestClass());

		assertThat(violations).hasSize(1);
		assertThat(violations.stream().findFirst().orElseThrow().getConstraintModel()).isNotNull();
		assertThat(violations.stream().findFirst().orElseThrow().getLocalizedMessage()).contains(randomString);
	}

	@Test
	public void shouldThrowExceptionWhenCheckingSameAttributeConstraintWithAttrDesc() throws InterceptorException
	{
		final AttributeConstraintModel existingConstraint = getDefaultAttributeConstraintWithAttributeDescriptor();
		modelService.save(existingConstraint);
		final AttributeConstraintModel constraint = getDefaultAttributeConstraintWithAttributeDescriptor();
		attributeConstraintPreparer.onPrepare(constraint, null);


		assertThat(constraint.getQualifier()).isEqualTo(existingConstraint.getQualifier());
		assertThat(constraint.isActive()).isEqualTo(existingConstraint.isActive());
		assertThat(constraint.getTarget()).isEqualTo(existingConstraint.getTarget());
		assertThat(constraint.getSeverity()).isEqualTo(existingConstraint.getSeverity());
		assertThat(emptyIfNull(constraint.getLanguages())).isEqualTo(existingConstraint.getLanguages());

		assertThatThrownBy(() -> validator.onValidate(constraint, null))
				.isNotNull()
				.isInstanceOf(InterceptorException.class)
				.hasMessageContaining(" Duplicated constrainted violation for an attribute constraint");
	}

	@Test
	public void shouldThrowExceptionWhenCheckingSameLocalizedAttributeConstraintWithAttrDescAndLanguages()
			throws InterceptorException
	{
		final AttributeConstraintModel existingConstraint = getDefaultLocalizedAttributeConstraintWithAttributeDescriptor();
		modelService.save(existingConstraint);
		final AttributeConstraintModel constraint = getDefaultLocalizedAttributeConstraintWithAttributeDescriptor();
		attributeConstraintPreparer.onPrepare(constraint, null);

		assertThat(constraint.getQualifier()).isEqualTo(existingConstraint.getQualifier());
		assertThat(constraint.isActive()).isEqualTo(existingConstraint.isActive());
		assertThat(constraint.getTarget()).isEqualTo(existingConstraint.getTarget());
		assertThat(constraint.getSeverity()).isEqualTo(existingConstraint.getSeverity());
		assertThat(emptyIfNull(constraint.getLanguages())).isEqualTo(existingConstraint.getLanguages());

		assertThatThrownBy(() -> validator.onValidate(constraint, null))
				.isNotNull()
				.isInstanceOf(InterceptorException.class)
				.hasMessageContaining(" Duplicated constrainted violation for an attribute constraint");
	}

	@Test
	public void shouldThrowExceptionWhenCheckingSameLocalizedAttributeConstraintWithAttrDescAndNoLanguages()
			throws InterceptorException
	{
		final AttributeConstraintModel existingConstraint = getDefaultLocalizedAttributeConstraintWithAttributeDescriptorNoLanguages();
		modelService.save(existingConstraint);
		final AttributeConstraintModel constraint = getDefaultLocalizedAttributeConstraintWithAttributeDescriptorNoLanguages();
		attributeConstraintPreparer.onPrepare(constraint, null);

		assertThat(constraint.getQualifier()).isEqualTo(existingConstraint.getQualifier());
		assertThat(constraint.isActive()).isEqualTo(existingConstraint.isActive());
		assertThat(constraint.getTarget()).isEqualTo(existingConstraint.getTarget());
		assertThat(constraint.getSeverity()).isEqualTo(existingConstraint.getSeverity());
		assertThat(constraint.getLanguages()).isEqualTo(existingConstraint.getLanguages());

		assertThatThrownBy(() -> validator.onValidate(constraint, null))
				.isNotNull()
				.isInstanceOf(InterceptorException.class)
				.hasMessageContaining(" Duplicated constrainted violation for an attribute constraint");
	}

	@Test
	public void shouldThrowExceptionWhenCheckingSameAttributeConstraintWithPOJO()
			throws InterceptorException
	{
		final AttributeConstraintModel existingConstraint = getDefaultAttributeConstraintWithPOJO();
		modelService.save(existingConstraint);
		final AttributeConstraintModel constraint = getDefaultAttributeConstraintWithPOJO();
		attributeConstraintPreparer.onPrepare(constraint, null);

		assertThat(constraint.getQualifier()).isEqualTo(existingConstraint.getQualifier());
		assertThat(constraint.isActive()).isEqualTo(existingConstraint.isActive());
		assertThat(constraint.getTarget()).isEqualTo(existingConstraint.getTarget());
		assertThat(constraint.getSeverity()).isEqualTo(existingConstraint.getSeverity());
		assertThat(emptyIfNull(constraint.getLanguages())).isEqualTo(existingConstraint.getLanguages());

		assertThatThrownBy(() -> validator.onValidate(constraint, null))
				.isNotNull()
				.isInstanceOf(InterceptorException.class)
				.hasMessageContaining(" Duplicated constrainted violation for an attribute constraint");
	}

	@Test
	public void shouldThrowExceptionWhenCheckingSameTypeConstraintWithPOJO()
			throws InterceptorException
	{
		final TypeConstraintModel existingConstraint = getDefaultTypeConstraintWithPOJO();
		modelService.save(existingConstraint);
		final TypeConstraintModel constraint = getDefaultTypeConstraintWithPOJO();
		typeConstraintPreparer.onPrepare(constraint, null);

		assertThat(constraint.isActive()).isEqualTo(existingConstraint.isActive());
		assertThat(constraint.getTarget()).isEqualTo(existingConstraint.getTarget());
		assertThat(constraint.getSeverity()).isEqualTo(existingConstraint.getSeverity());

		assertThatThrownBy(() -> validator.onValidate(constraint, null))
				.isNotNull()
				.isInstanceOf(InterceptorException.class)
				.hasMessageContaining(" Duplicated constrainted violation for a type constraint");
	}

	@Test
	public void shouldThrowExceptionWhenCheckingSameTypeConstraintWithItemModel()
			throws InterceptorException
	{
		final TypeConstraintModel existingConstraint = getDefaultTypeConstraintWithItemModel();
		modelService.save(existingConstraint);
		final TypeConstraintModel constraint = getDefaultTypeConstraintWithItemModel();
		typeConstraintPreparer.onPrepare(constraint, null);

		assertThat(constraint.isActive()).isEqualTo(existingConstraint.isActive());
		assertThat(constraint.getTarget()).isEqualTo(existingConstraint.getTarget());
		assertThat(constraint.getSeverity()).isEqualTo(existingConstraint.getSeverity());

		assertThatThrownBy(() -> validator.onValidate(constraint, null))
				.isNotNull()
				.isInstanceOf(InterceptorException.class)
				.hasMessageContaining(" Duplicated constrainted violation for a type constraint");
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithAttrDescAndDifferentQualifier() throws InterceptorException
	{
		testAttributeConstraintWithDifferentQualifier(this::getDefaultAttributeConstraintWithAttributeDescriptor,
				typeService.getAttributeDescriptor(TitleModel._TYPECODE, TitleModel.NAME));
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithAttrDescAndDifferentActiveFlag() throws InterceptorException
	{
		testAttributeConstraintWithDifferentActivityFlag(this::getDefaultAttributeConstraintWithAttributeDescriptor);
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithAttrDescAndDifferentTarget() throws InterceptorException
	{
		testAttributeConstraintWithDifferentTarget(this::getDefaultAttributeConstraintWithAttributeDescriptor,
				typeService.getAttributeDescriptor(AbstractOrderModel._TYPECODE, AbstractOrderModel.CODE));
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithAttrDescAndDifferentSeverity() throws InterceptorException
	{
		testAttributeConstraintWithDifferentSeverity(this::getDefaultAttributeConstraintWithAttributeDescriptor);
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithAttrDescAndDifferentLanguages() throws InterceptorException
	{
		testAttributeConstraintWithDifferentLanguages(this::getDefaultAttributeConstraintWithAttributeDescriptor);
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithLocalizedAttrDescAndDifferentQualifier()
			throws InterceptorException
	{
		testAttributeConstraintWithDifferentQualifier(this::getDefaultLocalizedAttributeConstraintWithAttributeDescriptor,
				typeService.getAttributeDescriptor(TitleModel._TYPECODE, TitleModel.CODE));
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithLocalizedAttrDescAndDifferentActiveFlag()
			throws InterceptorException
	{
		testAttributeConstraintWithDifferentActivityFlag(this::getDefaultLocalizedAttributeConstraintWithAttributeDescriptor);
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithLocalizedAttrDescAndDifferentTarget() throws InterceptorException
	{
		testAttributeConstraintWithDifferentTarget(this::getDefaultLocalizedAttributeConstraintWithAttributeDescriptor,
				typeService.getAttributeDescriptor(
						CatalogModel._TYPECODE, CatalogModel.NAME));
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithLocalizedAttrDescAndDifferentSeverity()
			throws InterceptorException
	{
		testAttributeConstraintWithDifferentSeverity(this::getDefaultLocalizedAttributeConstraintWithAttributeDescriptor);
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithLocalizedAttrDescAndDifferentLanguages()
			throws InterceptorException
	{
		testAttributeConstraintWithDifferentLanguages(this::getDefaultLocalizedAttributeConstraintWithAttributeDescriptor);
	}


	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithLocalizedAttrDescNoLangAndDifferentQualifier()
			throws InterceptorException
	{
		testAttributeConstraintWithDifferentQualifier(
				this::getDefaultLocalizedAttributeConstraintWithAttributeDescriptorNoLanguages,
				typeService.getAttributeDescriptor(TitleModel._TYPECODE, TitleModel.CODE));
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithLocalizedAttrDescNoLangAndDifferentActiveFlag()
			throws InterceptorException
	{
		testAttributeConstraintWithDifferentActivityFlag(
				this::getDefaultLocalizedAttributeConstraintWithAttributeDescriptorNoLanguages);
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithLocalizedAttrDescNoLangAndDifferentTarget()
			throws InterceptorException
	{
		testAttributeConstraintWithDifferentTarget(
				this::getDefaultLocalizedAttributeConstraintWithAttributeDescriptorNoLanguages,
				typeService.getAttributeDescriptor(CatalogModel._TYPECODE, CatalogModel.NAME));
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithLocalizedAttrDescNoLangAndDifferentSeverity()
			throws InterceptorException
	{
		testAttributeConstraintWithDifferentSeverity(
				this::getDefaultLocalizedAttributeConstraintWithAttributeDescriptorNoLanguages);
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithLocalizedAttrDescNoLangAndDifferentLanguages()
			throws InterceptorException
	{
		testAttributeConstraintWithDifferentLanguages(
				this::getDefaultLocalizedAttributeConstraintWithAttributeDescriptorNoLanguages);
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithPOJOAndDifferentQualifier() throws InterceptorException
	{
		testAttributeConstraintWithDifferentQualifier(this::getDefaultAttributeConstraintWithPOJO, "byteProperty");
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithPOJOAndDifferentActiveFlag() throws InterceptorException
	{
		testAttributeConstraintWithDifferentActivityFlag(this::getDefaultAttributeConstraintWithPOJO);
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithPOJOAndDifferentTarget() throws InterceptorException
	{
		testAttributeConstraintWithDifferentTarget(this::getDefaultAttributeConstraintWithPOJO, OtherTestClass.class);
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithPOJOAndDifferentSeverity() throws InterceptorException
	{
		testAttributeConstraintWithDifferentSeverity(this::getDefaultAttributeConstraintWithPOJO);
	}

	@Test
	public void shouldSuccessWhenCheckingAttributeConstraintWithPOJOAndDifferentLanguages() throws InterceptorException
	{
		testAttributeConstraintWithDifferentLanguages(this::getDefaultAttributeConstraintWithPOJO);
	}

	@Test
	public void shouldSuccessWhenCheckingTypeConstraintWithItemModelAndDifferentActiveFlag()
			throws InterceptorException
	{
		testTypeConstraintWithDifferentActiveFlag(this::getDefaultTypeConstraintWithItemModel);
	}

	@Test
	public void shouldSuccessWhenCheckingTypeConstraintWithItemModelAndDifferentTarget()
			throws InterceptorException
	{
		testTypeConstraintWithDifferentTarget(this::getDefaultTypeConstraintWithItemModel,
				c -> c.setType(typeService.getComposedTypeForClass(AbstractOrderModel.class)));
	}

	@Test
	public void shouldSuccessWhenCheckingTypeConstraintWithItemModelAndDifferentSeverity()
			throws InterceptorException
	{
		testTypeConstraintWithDifferentSeverity(this::getDefaultTypeConstraintWithItemModel);
	}

	@Test
	public void shouldSuccessWhenCheckingTypeConstraintWithPOJOAndDifferentActiveFlag()
			throws InterceptorException
	{
		testTypeConstraintWithDifferentActiveFlag(this::getDefaultTypeConstraintWithPOJO);
	}

	@Test
	public void shouldSuccessWhenCheckingTypeConstraintWithPOJOAndDifferentTarget()
			throws InterceptorException
	{
		testTypeConstraintWithDifferentTarget(this::getDefaultTypeConstraintWithPOJO,
				c -> c.setTarget(OtherTestClass.class));
	}

	@Test
	public void shouldSuccessWhenCheckingTypeConstraintWithPOJOAndDifferentSeverity()
			throws InterceptorException
	{
		testTypeConstraintWithDifferentSeverity(this::getDefaultTypeConstraintWithPOJO);
	}

	private void testTypeConstraintWithDifferentActiveFlag(final Supplier<TypeConstraintModel> constraintSupplier)
			throws InterceptorException
	{
		final TypeConstraintModel existingConstraint = constraintSupplier.get();
		modelService.save(existingConstraint);

		final TypeConstraintModel constraint = constraintSupplier.get();
		constraint.setActive(false);
		typeConstraintPreparer.onPrepare(constraint, null);

		assertThat(constraint.isActive()).isNotEqualTo(existingConstraint.isActive());
		assertThat(constraint.getTarget()).isEqualTo(existingConstraint.getTarget());
		assertThat(constraint.getSeverity()).isEqualTo(existingConstraint.getSeverity());

		validator.onValidate(constraint, null);

		//success
	}

	private void testTypeConstraintWithDifferentTarget(final Supplier<TypeConstraintModel> constraintSupplier,
	                                                   final Consumer<TypeConstraintModel> modification)
			throws InterceptorException
	{
		final TypeConstraintModel existingConstraint = constraintSupplier.get();
		modelService.save(existingConstraint);

		final TypeConstraintModel constraint = constraintSupplier.get();

		modification.accept(constraint);
		typeConstraintPreparer.onPrepare(constraint, null);

		assertThat(constraint.isActive()).isEqualTo(existingConstraint.isActive());
		assertThat(constraint.getTarget()).isNotEqualTo(existingConstraint.getTarget());
		assertThat(constraint.getSeverity()).isEqualTo(existingConstraint.getSeverity());

		validator.onValidate(constraint, null);

		//success
	}

	private void testTypeConstraintWithDifferentSeverity(final Supplier<TypeConstraintModel> constraintSupplier)
			throws InterceptorException
	{
		final TypeConstraintModel existingConstraint = constraintSupplier.get();
		modelService.save(existingConstraint);

		final TypeConstraintModel constraint = constraintSupplier.get();
		constraint.setSeverity(Severity.WARN);
		typeConstraintPreparer.onPrepare(constraint, null);

		assertThat(constraint.isActive()).isEqualTo(existingConstraint.isActive());
		assertThat(constraint.getTarget()).isEqualTo(existingConstraint.getTarget());
		assertThat(constraint.getSeverity()).isNotEqualTo(existingConstraint.getSeverity());

		validator.onValidate(constraint, null);

		//success
	}

	private void testAttributeConstraintWithDifferentQualifier(
			final Supplier<AttributeConstraintModel> constraintSupplier, final AttributeDescriptorModel attributeDescriptor)
			throws InterceptorException
	{
		testAttributeConstraintWithDifferentQualifier(constraintSupplier, c -> c.setDescriptor(attributeDescriptor));
	}


	private void testAttributeConstraintWithDifferentQualifier(
			final Supplier<AttributeConstraintModel> constraintSupplier, final String pojoProperty)
			throws InterceptorException
	{
		testAttributeConstraintWithDifferentQualifier(constraintSupplier, c -> c.setQualifier(pojoProperty));
	}

	private void testAttributeConstraintWithDifferentQualifier(
			final Supplier<AttributeConstraintModel> constraintSupplier, final Consumer<AttributeConstraintModel> modification)
			throws InterceptorException
	{
		final AttributeConstraintModel existingConstraint = constraintSupplier.get();
		modelService.save(existingConstraint);

		final AttributeConstraintModel constraint = constraintSupplier.get();

		modification.accept(constraint);
		attributeConstraintPreparer.onPrepare(constraint, null);

		assertThat(constraint.getQualifier()).isNotEqualTo(existingConstraint.getQualifier());
		assertThat(constraint.isActive()).isEqualTo(existingConstraint.isActive());
		assertThat(constraint.getTarget()).isEqualTo(existingConstraint.getTarget());
		assertThat(constraint.getSeverity()).isEqualTo(existingConstraint.getSeverity());
		assertThat(emptyIfNull(constraint.getLanguages())).isEqualTo(emptyIfNull(existingConstraint.getLanguages()));

		validator.onValidate(constraint, null);

		//success
	}

	private void testAttributeConstraintWithDifferentActivityFlag(final Supplier<AttributeConstraintModel> constraintSupplier)
			throws InterceptorException
	{
		final AttributeConstraintModel existingConstraint = constraintSupplier.get();
		modelService.save(existingConstraint);

		final AttributeConstraintModel constraint = constraintSupplier.get();

		constraint.setActive(false);
		attributeConstraintPreparer.onPrepare(constraint, null);

		assertThat(constraint.getQualifier()).isEqualTo(existingConstraint.getQualifier());
		assertThat(constraint.isActive()).isNotEqualTo(existingConstraint.isActive());
		assertThat(constraint.getTarget()).isEqualTo(existingConstraint.getTarget());
		assertThat(constraint.getSeverity()).isEqualTo(existingConstraint.getSeverity());
		assertThat(emptyIfNull(constraint.getLanguages())).isEqualTo(emptyIfNull(existingConstraint.getLanguages()));

		validator.onValidate(constraint, null);

		//success
	}

	private void testAttributeConstraintWithDifferentTarget(
			final Supplier<AttributeConstraintModel> constraintSupplier, final AttributeDescriptorModel descriptor)
			throws InterceptorException
	{
		testAttributeConstraintWithDifferentTarget(constraintSupplier, c -> c.setDescriptor(descriptor));
	}

	private void testAttributeConstraintWithDifferentTarget(
			final Supplier<AttributeConstraintModel> constraintSupplier, final Class target)
			throws InterceptorException
	{
		testAttributeConstraintWithDifferentTarget(constraintSupplier, c -> c.setTarget(target));
	}

	private void testAttributeConstraintWithDifferentTarget(
			final Supplier<AttributeConstraintModel> constraintSupplier, final Consumer<AttributeConstraintModel> modification)
			throws InterceptorException
	{
		final AttributeConstraintModel existingConstraint = constraintSupplier.get();
		modelService.save(existingConstraint);

		final AttributeConstraintModel constraint = constraintSupplier.get();

		modification.accept(constraint);
		attributeConstraintPreparer.onPrepare(constraint, null);

		assertThat(constraint.getQualifier()).isEqualTo(existingConstraint.getQualifier());
		assertThat(constraint.isActive()).isEqualTo(existingConstraint.isActive());
		assertThat(constraint.getTarget()).isNotEqualTo(existingConstraint.getTarget());
		assertThat(constraint.getSeverity()).isEqualTo(existingConstraint.getSeverity());
		assertThat(emptyIfNull(constraint.getLanguages())).isEqualTo(emptyIfNull(existingConstraint.getLanguages()));

		validator.onValidate(constraint, null);

		//success
	}

	private void testAttributeConstraintWithDifferentSeverity(final Supplier<AttributeConstraintModel> constraintSupplier)
			throws InterceptorException
	{
		final AttributeConstraintModel existingConstraint = constraintSupplier.get();
		modelService.save(existingConstraint);

		final AttributeConstraintModel constraint = constraintSupplier.get();

		constraint.setSeverity(Severity.WARN);
		attributeConstraintPreparer.onPrepare(constraint, null);

		assertThat(constraint.getQualifier()).isEqualTo(existingConstraint.getQualifier());
		assertThat(constraint.isActive()).isEqualTo(existingConstraint.isActive());
		assertThat(constraint.getTarget()).isEqualTo(existingConstraint.getTarget());
		assertThat(constraint.getSeverity()).isNotEqualTo(existingConstraint.getSeverity());
		assertThat(emptyIfNull(constraint.getLanguages())).isEqualTo(emptyIfNull(existingConstraint.getLanguages()));

		validator.onValidate(constraint, null);

		//success
	}

	private void testAttributeConstraintWithDifferentLanguages(final Supplier<AttributeConstraintModel> constraintSupplier)
			throws InterceptorException
	{
		final AttributeConstraintModel existingConstraint = constraintSupplier.get();
		modelService.save(existingConstraint);

		final AttributeConstraintModel constraint = constraintSupplier.get();

		constraint.setLanguages(Set.of(english));
		attributeConstraintPreparer.onPrepare(constraint, null);

		assertThat(constraint.getQualifier()).isEqualTo(existingConstraint.getQualifier());
		assertThat(constraint.isActive()).isEqualTo(existingConstraint.isActive());
		assertThat(constraint.getTarget()).isEqualTo(existingConstraint.getTarget());
		assertThat(constraint.getSeverity()).isEqualTo(existingConstraint.getSeverity());
		assertThat(emptyIfNull(constraint.getLanguages())).isNotEqualTo(emptyIfNull(existingConstraint.getLanguages()));

		validator.onValidate(constraint, null);

		//success
	}


	private AttributeConstraintModel getDefaultAttributeConstraintWithAttributeDescriptor()
	{
		final AttributeConstraintModel o = getAttributeConstraintModel();

		o.setDescriptor(typeService.getAttributeDescriptor(TitleModel._TYPECODE, Title.CODE));

		modelService.detach(o);

		return o;
	}

	private AttributeConstraintModel getDefaultLocalizedAttributeConstraintWithAttributeDescriptor()
	{
		final AttributeConstraintModel o = getAttributeConstraintModel();

		final ComposedTypeModel titleComposedType = typeService.getComposedTypeForClass(TitleModel.class);
		o.setDescriptor(typeService.getAttributeDescriptor(titleComposedType, Title.NAME));
		o.setLanguages(Set.of(english, german));

		modelService.detach(o);

		return o;
	}

	private AttributeConstraintModel getDefaultLocalizedAttributeConstraintWithAttributeDescriptorNoLanguages()
	{
		final AttributeConstraintModel o = getDefaultLocalizedAttributeConstraintWithAttributeDescriptor();

		modelService.detach(o);

		return o;
	}


	private AttributeConstraintModel getDefaultAttributeConstraintWithPOJO()
	{
		final AttributeConstraintModel o = getAttributeConstraintModel();

		o.setTarget(TestClass.class);
		o.setQualifier("stringProperty");

		modelService.detach(o);

		return o;
	}

	private AttributeConstraintModel getAttributeConstraintModel()
	{
		final AttributeConstraintModel o = modelService.create(NotNullConstraintModel.class);

		o.setId(UUID.randomUUID().toString());
		o.setAnnotation(NotNull.class);
		o.setActive(true);
		o.setSeverity(Severity.ERROR);
		return o;
	}

	private TypeConstraintModel getDefaultTypeConstraintWithItemModel()
	{
		final XorNullReferenceConstraintModel o = modelService.create(XorNullReferenceConstraintModel.class);

		o.setId(UUID.randomUUID().toString());
		o.setActive(true);
		o.setSeverity(Severity.ERROR);
		o.setAnnotation(XorNotNull.class);

		o.setType(typeService.getComposedTypeForClass(TitleModel.class));
		o.setFirstFieldName(TitleModel.CODE);
		o.setSecondFieldName(TitleModel.NAME);

		modelService.detach(o);

		return o;
	}

	private TypeConstraintModel getDefaultTypeConstraintWithPOJO()
	{
		final XorNullReferenceConstraintModel o = modelService.create(XorNullReferenceConstraintModel.class);

		o.setId(UUID.randomUUID().toString());
		o.setActive(true);
		o.setSeverity(Severity.ERROR);
		o.setAnnotation(XorNotNull.class);

		o.setTarget(TestClass.class);
		o.setFirstFieldName("byteProperty");
		o.setSecondFieldName("stringProperty");

		modelService.detach(o);

		return o;
	}
}
