/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.scripting.engine.repository.impl;

import javax.annotation.PostConstruct;


public class TestClasspathScriptsRepository extends ClasspathScriptsRepository
{
	@Override
	@PostConstruct
	public void init()
	{
		tenantId = "junit";
	}
}
