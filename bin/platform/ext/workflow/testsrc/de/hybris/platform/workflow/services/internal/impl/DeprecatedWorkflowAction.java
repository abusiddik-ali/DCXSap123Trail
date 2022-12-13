/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.workflow.services.internal.impl;

import de.hybris.platform.workflow.jalo.AutomatedWorkflowTemplateJob;
import de.hybris.platform.workflow.jalo.WorkflowAction;
import de.hybris.platform.workflow.jalo.WorkflowDecision;


/**
 * Testing class for {@link DefaultAutomatedWorkflowTemplateRegistryTest} test case
 */
public class DeprecatedWorkflowAction implements AutomatedWorkflowTemplateJob
{

	@Override
	public WorkflowDecision perform(final WorkflowAction action)
	{
		return null;
	}

}
