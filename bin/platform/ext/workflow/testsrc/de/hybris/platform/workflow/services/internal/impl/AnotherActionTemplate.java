/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.workflow.services.internal.impl;

/**
 * Testing class for {@link DefaultAutomatedWorkflowTemplateRegistryTest} test case
 */
public final class AnotherActionTemplate
{
	/**
	 * Private constructor to force IllegalAccessException during reflective instantiation.
	 */
	private AnotherActionTemplate()
	{
		super();
	}

	public static AnotherActionTemplate getInstance()
	{
		return new AnotherActionTemplate();
	}
}
