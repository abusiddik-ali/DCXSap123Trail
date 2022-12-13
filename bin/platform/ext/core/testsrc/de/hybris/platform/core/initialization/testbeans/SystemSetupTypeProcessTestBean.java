/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core.initialization.testbeans;

import de.hybris.platform.core.initialization.SystemSetup;
import de.hybris.platform.core.initialization.SystemSetup.Process;
import de.hybris.platform.core.initialization.SystemSetup.Type;


/**
 * This bean acts like a real system setup bean but is not part of the normal application context
 */

@SystemSetup(extension = SystemSetupTypeProcessTestBean.SYSTEM_SETUP_TYPE_PROCESS_TEST_EXTENSION)
public class SystemSetupTypeProcessTestBean
{
	public static final String SYSTEM_SETUP_TYPE_PROCESS_TEST_EXTENSION = "SystemSetupTypeBeanTestExtension";

	public static final String ESSENTIAL_INIT = "essentialinit";
	public static final String ESSENTIAL_UPDATE = "essentialupdate";
	public static final String PROJECT_INIT = "projectinit";
	public static final String PROJECT_UPDATE = "projectupdate";

	@SystemSetup(type = Type.ESSENTIAL, process = Process.INIT)
	public void essentialInit() throws Exception
	{
		throw new Exception(ESSENTIAL_INIT);
	}

	@SystemSetup(type = Type.ESSENTIAL, process = Process.UPDATE)
	public void essentialUpdate() throws Exception
	{
		throw new Exception(ESSENTIAL_UPDATE);
	}

	@SystemSetup(type = Type.PROJECT, process = Process.INIT)
	public void projectInit() throws Exception
	{
		throw new Exception(PROJECT_INIT);
	}

	@SystemSetup(type = Type.PROJECT, process = Process.UPDATE)
	public void projectUpdate() throws Exception
	{
		throw new Exception(PROJECT_UPDATE);
	}
}
