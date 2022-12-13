/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util.logging;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.core.Log4JUtils;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.Test;


public class LogsTest
{
	static
	{
		Log4JUtils.startup();
	}

	private static Logger LOG = Logger.getLogger(LogsTest.class);

	private boolean calculateSomethingCalled;

	@Before
	public void setUp()
	{
		calculateSomethingCalled = false;
	}

	@Test
	public void shouldNotEvaluateLambdaWhenDebugIsDisabled()
	{
		LOG.setLevel(Level.INFO);
		Logs.debug(LOG, () -> "Something " + calculateSomething() + " is wrong");

		assertThat(calculateSomethingCalled).isFalse();
	}

	@Test
	public void shouldEvaluateLambdaWhenDebugIsEnabled()
	{
		LOG.setLevel(Level.DEBUG);

		Logs.debug(LOG, () -> "Something " + calculateSomething() + " is wrong");

		assertThat(calculateSomethingCalled).isTrue();
	}

	private String calculateSomething()
	{
		calculateSomethingCalled = true;
		return "TEST";
	}
}
