/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.testframework;

import org.assertj.core.api.Assertions;
import org.junit.Test;

public class SampleSuccessTest
{
	@Test
	public void test()
	{
		Assertions.assertThat(true).isTrue();
	}
}
