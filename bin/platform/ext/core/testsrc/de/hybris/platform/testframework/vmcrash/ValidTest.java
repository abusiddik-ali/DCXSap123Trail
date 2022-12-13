/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.testframework.vmcrash;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.ManualTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import org.junit.Test;

@ManualTest
public class ValidTest extends ServicelayerBaseTest
{

	@Test
	public void validTest()
	{
		final int i = 2+ 2;
		assertThat(i).isEqualTo(4);
	}
}
