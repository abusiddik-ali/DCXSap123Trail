/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core.initialization.testbeans;

import de.hybris.platform.core.initialization.SystemSetup;


@SystemSetup(extension = SystemSetupEmptyTestBean.NOTHIN_TEST_EXTENSION)
public class SystemSetupEmptyTestBean
{
	public static final String NOTHIN_TEST_EXTENSION = "nothin_extension";

	public void doNothing()
	{
		//nope
	}
}
