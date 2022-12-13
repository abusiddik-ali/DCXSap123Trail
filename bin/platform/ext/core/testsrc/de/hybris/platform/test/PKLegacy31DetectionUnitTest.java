/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.Tenant;

import org.junit.Test;


@UnitTest
public class PKLegacy31DetectionUnitTest
{

	@Test
	public void testGetCounterWithoutTenant()
	{
		final PK dummy = PK.createFixedCounterPK(1, 1234);
		final Tenant currentTenant = Registry.getCurrentTenantNoFallback();
		try
		{
			Registry.unsetCurrentTenant();

			dummy.getCounter(); // boom
		}
		finally
		{
			if (currentTenant != null)
			{
				Registry.setCurrentTenant(currentTenant);
			}
		}
	}

}
