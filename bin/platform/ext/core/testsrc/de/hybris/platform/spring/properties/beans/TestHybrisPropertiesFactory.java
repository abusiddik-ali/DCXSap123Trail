/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.spring.properties.beans;

import de.hybris.platform.servicelayer.config.impl.HybrisPropertiesFactory;

import java.util.Map;
import java.util.Properties;


/**
 *
 */
public class TestHybrisPropertiesFactory extends HybrisPropertiesFactory
{
	private Map<String, String> allProps;


	public void setAllProps(final Map<String, String> allProps)
	{
		this.allProps = allProps;
	}


	@Override
	public Properties getObject()
	{
		final Properties props = new Properties();
		props.putAll(allProps);
		return props;
	}

}
