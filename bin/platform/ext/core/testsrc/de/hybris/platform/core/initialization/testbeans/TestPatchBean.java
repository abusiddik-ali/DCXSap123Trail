/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core.initialization.testbeans;

import de.hybris.platform.constants.CoreConstants;
import de.hybris.platform.core.initialization.SystemSetup;


@SystemSetup(extension = CoreConstants.EXTENSIONNAME)
public class TestPatchBean
{
	@SystemSetup(patch = true)
	public void requiredPatch()
	{
	}

	@SystemSetup(patch = true)
	public void optionalPatch()
	{
	}
}
