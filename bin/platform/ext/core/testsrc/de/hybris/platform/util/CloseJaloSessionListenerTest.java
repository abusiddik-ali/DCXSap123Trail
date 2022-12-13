/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util;

import de.hybris.bootstrap.annotations.UnitTest;

import javax.servlet.http.HttpSessionEvent;

import org.junit.Test;
import org.springframework.mock.web.MockHttpSession;


@UnitTest
public class CloseJaloSessionListenerTest
{

	@Test
	public void shouldNotThrowIllegalStateExceptionWhenNoTenantIsActive() //see PLA-12995
	{
		final HttpSessionEvent event = new HttpSessionEvent(new MockHttpSession());
		new CloseJaloSessionListener().sessionDestroyed(event);
	}
}
