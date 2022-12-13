/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.testframework.vmcrash;

import de.hybris.bootstrap.annotations.ManualTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import org.junit.Test;

@ManualTest
public class VMCrashingTest extends ServicelayerBaseTest
{

	@Test
	public void crashVm()
	{
		System.exit(1);
	}
}
