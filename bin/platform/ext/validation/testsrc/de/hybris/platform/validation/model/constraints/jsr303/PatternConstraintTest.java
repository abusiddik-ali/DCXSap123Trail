/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.validation.model.constraints.jsr303;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.interceptor.impl.MandatoryAttributesValidator;
import de.hybris.platform.servicelayer.interceptor.impl.MandatoryAttributesValidator.MissingMandatoryAttributesException;
import de.hybris.platform.validation.enums.RegexpFlag;

import java.util.Collections;

import org.junit.Test;


/**
 * Testing Regexp Constraint
 */
@IntegrationTest
public class PatternConstraintTest extends AbstractConstraintTest
{

	private void createPattenConstraint()
	{
		final PatternConstraintModel constraint = modelService.create(PatternConstraintModel.class);
		constraint.setId("patternConstraint");
		constraint.setQualifier("field");
		constraint.setTarget(POJO.class);
		constraint.setRegexp("code.*code");
		constraint.setFlags(Collections.singleton(RegexpFlag.DOTALL));
		modelService.saveAll(constraint);
		assertThat(constraint.getDefaultMessage()).isEqualTo(getDefaultMessage(Constraint.PATTERN.msgKey));
		validationService.reloadValidationEngine();
	}

	/**
	 * lack of the regexp flag
	 */
	@Test
	public void testInvalidCreate() throws InterruptedException
	{
		final POJO xxx = new POJO();
		xxx.setField("codehahahahahacode");

		final PatternConstraintModel constraint = new PatternConstraintModel();
		constraint.setId("patternConstraint");
		constraint.setQualifier("field");
		constraint.setTarget(POJO.class);
		constraint.setRegexp("code.*code");
		try
		{
			modelService.saveAll(constraint);
		}
		catch (final Exception e)
		{
			checkException(e, ModelSavingException.class, MissingMandatoryAttributesException.class,
					MandatoryAttributesValidator.class);
		}

	}

	/**
	 * lack of the regexp flag
	 */
	@Test
	public void testInvalidCreate2() throws InterruptedException
	{
		final POJO xxx = new POJO();
		xxx.setField("codehahahahahacode");

		final PatternConstraintModel constraint = new PatternConstraintModel();
		constraint.setId("patternConstraint");
		constraint.setQualifier("field");
		constraint.setTarget(POJO.class);
		constraint.setFlags(Collections.singleton(RegexpFlag.DOTALL));

		try
		{
			modelService.saveAll(constraint);
		}
		catch (final Exception e)
		{
			checkException(e, ModelSavingException.class, MissingMandatoryAttributesException.class,
					MandatoryAttributesValidator.class);
		}
	}

	@Test
	public void testRegexpOK1() throws InterruptedException
	{
		final POJO xxx = new POJO();
		xxx.setField("codehahahahahacode");
		createPattenConstraint();
		assertThat(validationService.validate(xxx)).isEmpty();
	}

	@Test
	public void testRegexpOk2() throws InterruptedException
	{
		final POJO xxx = new POJO();
		xxx.setField("codecode");
		createPattenConstraint();
		assertThat(validationService.validate(xxx)).isEmpty();
	}

	@Test
	public void testRegexpFalse1() throws InterruptedException
	{
		createPattenConstraint();
		final POJO xxx = new POJO();
		xxx.setField("codesomedatacode[wrong]");
		assertThat(validationService.validate(xxx)).isNotEmpty();
	}

	@Test
	public void testRegexpFalse2() throws InterruptedException
	{
		createPattenConstraint();
		final POJO xxx = new POJO();
		xxx.setField("[bad]codesomedatacode");
		assertThat(validationService.validate(xxx)).isNotEmpty();
	}

	@Test
	public void testRegexpNull() throws InterruptedException
	{
		createPattenConstraint();
		final POJO xxx = new POJO();
		assertThat(validationService.validate(xxx)).isEmpty();
	}


	protected class POJO
	{
		private String field;

		/**
		 * @return the field
		 */
		public String getField()
		{
			return field;
		}

		/**
		 * @param field the field to set
		 */
		public void setField(final String field)
		{
			this.field = field;
		}
	}
}
