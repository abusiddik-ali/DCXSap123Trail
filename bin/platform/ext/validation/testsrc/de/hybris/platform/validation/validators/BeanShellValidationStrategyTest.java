/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.validation.validators;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;

import javax.annotation.Resource;

import org.junit.Test;

import bsh.Capabilities;

@IntegrationTest
public class BeanShellValidationStrategyTest extends ServicelayerBaseTest
{
	@Resource
	ModelService modelService;

	@Test
	public void shouldValidateNegativelyWhenAccessibilityIsEnabled() throws Capabilities.Unavailable
	{
		verify(true, false);
	}

	@Test
	public void shouldValidateNegativelyWhenAccessibilityIsDisabled() throws Capabilities.Unavailable
	{
		verify(false, false);
	}

	@Test
	public void shouldValidatePositivelyWhenAccessibilityIsEnabled() throws Capabilities.Unavailable
	{
		verify(true, true);
	}

	@Test
	public void shouldValidatePositivelyWhenAccessibilityIsDisabled() throws Capabilities.Unavailable
	{
		verify(false, true);
	}

	private void verify(final boolean accessibilityFlag, final boolean expectedResult) throws Capabilities.Unavailable
	{
		Capabilities.setAccessibility(accessibilityFlag);
		final TitleModel titleToValidate = modelService.create(TitleModel.class);

		final BeanShellValidationStrategy validationStrategy = new BeanShellValidationStrategy();
		final String expression = expectedResult ? "ctx.getBean(\"modelService\") != null && getCode() == null" : "ctx.getBean(\"modelService\") != null && getCode() != null";
		final boolean validationResult = validationStrategy.validate(expression, titleToValidate);

		assertThat(validationResult).isEqualTo(expectedResult);
	}
}