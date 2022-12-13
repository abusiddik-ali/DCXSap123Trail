/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.validation.model.constraints;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.cronjob.model.JobModel;
import de.hybris.platform.cronjob.model.TriggerModel;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.impl.MandatoryAttributesValidator;
import de.hybris.platform.servicelayer.interceptor.impl.MandatoryAttributesValidator.MissingMandatoryAttributesException;
import de.hybris.platform.validation.annotations.XorNotNull;
import de.hybris.platform.validation.exceptions.HybrisConstraintViolation;
import de.hybris.platform.validation.interceptors.XorNullReferenceConstraintValidator;
import de.hybris.platform.validation.model.constraints.jsr303.AbstractConstraintTest;

import java.util.Set;

import org.junit.Assert;
import org.junit.Test;


/**
 * Tests covering {@link XorNullReferenceConstraintModel} implementation.
 */
@IntegrationTest
public class XorNullReferenceConstraintTest extends AbstractConstraintTest
{
	/**
	 * validates {@link XorNotNull} if the field mapping is appropriate
	 */
	@Test
	public void validateXorNullReferenceConstraint1()
	{

		final XorNullReferenceConstraintModel xorConstraint = modelService.create(XorNullReferenceConstraintModel.class);
		xorConstraint.setId("xorConstraint");
		final ComposedTypeModel triggerCTModel = typeService.getComposedType(TriggerModel.class);
		xorConstraint.setType(triggerCTModel);
		xorConstraint.setAnnotation(XorNotNull.class);
		xorConstraint.setFirstFieldName("notASinglejob");
		xorConstraint.setSecondFieldName("aSingleCronjob");
		try
		{
			modelService.save(xorConstraint);
		}
		catch (final Exception e)
		{
			checkException(e, ModelSavingException.class, InterceptorException.class, XorNullReferenceConstraintValidator.class);
		}
	}

	/**
	 * validates {@link XorNotNull} if the field mapping is appropriate , null mapping case
	 */
	@Test
	public void validateXorNullReferenceConstraint2()
	{

		final XorNullReferenceConstraintModel xorConstraint = modelService.create(XorNullReferenceConstraintModel.class);
		xorConstraint.setId("xorConstraint");
		final ComposedTypeModel triggerCTModel = typeService.getComposedType(TriggerModel.class);
		xorConstraint.setType(triggerCTModel);
		xorConstraint.setAnnotation(XorNotNull.class);
		xorConstraint.setFirstFieldName("job");
		try
		{
			modelService.save(xorConstraint);
		}
		catch (final Exception e)
		{
			checkException(e, ModelSavingException.class, MissingMandatoryAttributesException.class,
					MandatoryAttributesValidator.class);
		}
	}

	/**
	 * validates using {@link XorNotNull} annotation
	 */
	@Test
	public void validateXorNullReferenceTriggerType()
	{
		final XorNullReferenceConstraintModel xorConstraint = modelService.create(XorNullReferenceConstraintModel.class);
		xorConstraint.setId("xorConstraint");
		final ComposedTypeModel triggerCTModel = typeService.getComposedType(TriggerModel.class);
		xorConstraint.setType(triggerCTModel);
		xorConstraint.setAnnotation(XorNotNull.class);
		xorConstraint.setFirstFieldName("job");
		xorConstraint.setSecondFieldName("cronJob");

		modelService.save(xorConstraint);
		Assert.assertEquals(getDefaultMessage(Constraint.XOR_NOT_NULL_REFERENCE.msgKey), xorConstraint.getDefaultMessage());
		validationService.reloadValidationEngine();

		final TriggerModel triggerModel = modelService.create(TriggerModel.class);
		triggerModel.setJob(null);
		triggerModel.setCronJob(null);

		Set<HybrisConstraintViolation> constraintViolations = validationService.validate(triggerModel);

		assertThat(constraintViolations).describedAs("Trigger with both job/cronjob as nulls should not validate correctly ")
		                                .hasSize(1);

		triggerModel.setJob(new JobModel());
		constraintViolations = validationService.validate(triggerModel);

		assertThat(constraintViolations).describedAs(
				"Trigger with either job or cronjob as null(but not both at once) should validate correctly ").isEmpty();

		triggerModel.setCronJob(new CronJobModel());

		constraintViolations = validationService.validate(triggerModel);

		assertThat(constraintViolations).describedAs("Trigger with both job/cronjob as not null should not validate correctly ")
		                                .hasSize(1);

		assertThat(constraintViolations).describedAs("Should violate specific dynamic constraint")
		                                .extracting(HybrisConstraintViolation::getMessageTemplate)
		                                .contains("{" + Constraint.XOR_NOT_NULL_REFERENCE.msgKey + "}");
	}
}
