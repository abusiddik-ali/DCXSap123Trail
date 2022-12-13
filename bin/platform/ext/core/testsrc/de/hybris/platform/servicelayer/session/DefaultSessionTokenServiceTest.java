/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.session;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.servicelayer.session.impl.DefaultSessionTokenService;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;


@UnitTest
public class DefaultSessionTokenServiceTest
{
	private DefaultSessionTokenService sessionTokenService;

	@Before
	public void setup()
	{
		sessionTokenService = new DefaultSessionTokenService();
		sessionTokenService.setSessionService(new MockSessionService());
	}

	@Test
	public void testCreateToken()
	{
		//when
		final String token = sessionTokenService.getOrCreateSessionToken();

		//then
		Assert.assertNotNull("Got null as token", token);
		Assert.assertTrue("Got empty token", token.length() > 0);
	}

	@Test
	public void testSetToken()
	{
		//given
		final String expectedToken = "1234567890";

		sessionTokenService.setSessionToken(expectedToken);

		//when
		final String token = sessionTokenService.getOrCreateSessionToken();

		//then
		Assert.assertEquals("Returned token is different than set token", expectedToken, token);
	}

}
