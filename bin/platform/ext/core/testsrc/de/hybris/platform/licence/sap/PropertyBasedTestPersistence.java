/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.licence.sap;

import de.hybris.bootstrap.config.ConfigUtil;

import java.io.File;

import org.apache.commons.io.FileUtils;

/**
 * Property based persistence for testing purposes.
 */
public class PropertyBasedTestPersistence extends DefaultPersistence
{
	private static final String FILE_NAME = ConfigUtil.getPlatformConfig(DefaultPersistence.class).getSystemConfig().getTempDir()
			+ "/testPersistence.properties";

	@Override
	String getPropsFileName()
	{
		return FILE_NAME;
	}

	public void removePersistenceFile()
	{
		final File file = new File(FILE_NAME);
		FileUtils.deleteQuietly(file);
	}
}
