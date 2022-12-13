/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.workflow.interceptors;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.workflow.model.WorkflowTemplateModel;

import javax.annotation.Resource;

import org.junit.Test;


@IntegrationTest
public class WorkflowTemplateDefaultCodeInterceptorTest extends ServicelayerTest
{
	@Resource
	private ModelService modelService;

	@Test
	public void testIfCodeIsNotNull()
	{
		final WorkflowTemplateModel decision = modelService.create(WorkflowTemplateModel.class);

		modelService.save(decision);

		assertThat(decision.getCode()).isNotNull();
		assertThat(decision.getCode()).hasSize(6);
	}
}
