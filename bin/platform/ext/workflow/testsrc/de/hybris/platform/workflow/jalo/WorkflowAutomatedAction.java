/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.workflow.jalo;

import org.apache.log4j.Logger;


/**
 * @deprecated since ages - as of release 4.3, please use{@link de.hybris.platform.workflow.jobs.WorkflowAutomatedAction}
 */
@Deprecated(since = "ages", forRemoval = false)
public class WorkflowAutomatedAction implements AutomatedWorkflowTemplateJob
{
	private static final Logger LOG = Logger.getLogger(WorkflowAutomatedAction.class.getName());

	@Override
	public WorkflowDecision perform(final WorkflowAction action)
	{
		LOG.info("This will complete the action automatically without any user interaction");

		for (final WorkflowDecision decision : action.getDecisions())
		{
			return decision;
		}
		return null;
	}

}
