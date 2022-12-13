/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.processengine.definition;


public class TestProcessDefinitionsProvider extends ProcessDefinitionsProvider
{
	public TestProcessDefinitionsProvider(final XMLProcessDefinitionsReader xmlDefinitionsReader)
	{
		super(xmlDefinitionsReader, null);
	}

	@Override
	public ProcessDefinition getDefinition(final ProcessDefinitionId id)
	{
		return null;
	}

	@Override
	public ProcessDefinitionId getLatestDefinitionIdFor(final ProcessDefinitionId id)
	{
		return null;
	}
}
