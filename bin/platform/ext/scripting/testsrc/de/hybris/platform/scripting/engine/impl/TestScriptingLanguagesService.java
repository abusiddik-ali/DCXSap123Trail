/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.scripting.engine.impl;

import org.springframework.context.ApplicationContext;


public class TestScriptingLanguagesService extends DefaultScriptingLanguagesService
{
	private ApplicationContext applicationContext;

	@Override
	ApplicationContext getApplicationContext()
	{
		return applicationContext;
	}

	public void setApplicationContext(final ApplicationContext applicationContext)
	{
		this.applicationContext = applicationContext;
	}
}
