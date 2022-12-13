/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test.structure;


import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.bootstrap.config.ConfigUtil;

import org.apache.log4j.Logger;


@IntegrationTest
public class PlatformExtensionsProjectStructureTest extends AbstractProjectStructureTest
{

	static final Logger log = Logger.getLogger(PlatformExtensionsProjectStructureTest.class.getName());

	public PlatformExtensionsProjectStructureTest()
	{
		super(ConfigUtil.getPlatformConfig(PlatformExtensionsProjectStructureTest.class).getAllPlatformExtensionNames()
		                .toArray(new String[0]));
	}
}

