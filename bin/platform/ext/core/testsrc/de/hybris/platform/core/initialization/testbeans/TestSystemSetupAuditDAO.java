/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core.initialization.testbeans;

import de.hybris.platform.core.initialization.SystemSetupAuditDAO;
import de.hybris.platform.core.initialization.SystemSetupCollectorResult;
import de.hybris.platform.core.model.initialization.SystemSetupAuditModel;


public class TestSystemSetupAuditDAO implements SystemSetupAuditDAO
{

	@Override
	public boolean isPatchApplied(final String patchHash)
	{
		return false;
	}

	@Override
	public SystemSetupAuditModel storeSystemPatchAction(final SystemSetupCollectorResult collectorResult)
	{
		return null;
	}
}
