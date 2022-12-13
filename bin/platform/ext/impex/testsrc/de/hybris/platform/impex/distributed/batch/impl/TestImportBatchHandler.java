/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.impex.distributed.batch.impl;

import de.hybris.platform.impex.distributed.batch.ImportBatchHandler;

import java.util.HashMap;
import java.util.Map;


/**
 * This is test implementation.
 */
public class TestImportBatchHandler implements ImportBatchHandler
{
	private final Map<String, String> properties;
	private final String inputData;
	private String outputData;
	private long remainingWorkLoad;

	public TestImportBatchHandler(final String inputData)
	{
		this.inputData = inputData;
		this.properties = new HashMap<>();
	}


	@Override
	public String getInputData()
	{
		return inputData;
	}

	@Override
	public void setOutputData(final String data)
	{
		this.outputData = data;
	}

	@Override
	public void setRemainingWorkLoad(final long remainingWorkLoad)
	{
		this.remainingWorkLoad = remainingWorkLoad;
	}

	@Override
	public String getProperty(final String propertyName)
	{
		return properties.get(propertyName);
	}

	@Override
	public void setProperty(final String propertyName, final String propertyValue)
	{
		properties.put(propertyName, propertyValue);
	}

	public String getOutputData()
	{
		return outputData;
	}

	public long getRemainingWorkLoad()
	{
		return remainingWorkLoad;
	}
}
