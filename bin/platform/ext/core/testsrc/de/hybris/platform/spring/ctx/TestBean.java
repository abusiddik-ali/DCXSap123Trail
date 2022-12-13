/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.spring.ctx;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;


public class TestBean
{
	@Autowired
	private Map<String, TestSubBean> mapping;

	private String simpleProperty;
	private TestBean beanProperty;

	/**
	 * @param simpleProperty the simpleProperty to set
	 */
	public void setSimpleProperty(final String simpleProperty)
	{
		this.simpleProperty = simpleProperty;
	}


	/**
	 * @return the simpleProperty
	 */
	public String getSimpleProperty()
	{
		return simpleProperty;
	}

	/**
	 * @param beanProperty the beanProperty to set
	 */
	public void setBeanProperty(final TestBean beanProperty)
	{
		this.beanProperty = beanProperty;
	}

	/**
	 * @return the beanProperty
	 */
	public TestBean getBeanProperty()
	{
		return beanProperty;
	}


	/**
	 * @return the mapping
	 */
	public Map<String, TestSubBean> getMapping()
	{
		return mapping;
	}


	/**
	 * @param mapping the mapping to set
	 */
	public void setMapping(final Map<String, TestSubBean> mapping)
	{
		this.mapping = mapping;
	}

}
