/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.bootstrap.ddl;

import de.hybris.bootstrap.annotations.UnitTest;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

@UnitTest
public class InitUserPasswordServiceTest
{
	private InitUserPasswordService initUserPasswordService;

	@Before
	public void prepareTest()
	{
		initUserPasswordService = new InitUserPasswordService(getMockedPropertiesLoader());
	}

	@Test
	public void shouldReturnUserPassword()
	{
		final String adminPassword = initUserPasswordService.readUserPassword("admin");
		final String userPassword = initUserPasswordService.readUserPassword("user");
		final String anonymousPassword = initUserPasswordService.readUserPassword("anonymous");

		assertThat(adminPassword).isEqualTo("nimda");
		assertThat(userPassword).isEqualTo("resu");
		assertThat(anonymousPassword).isEqualTo("suomynona");
	}

	@Test(expected = IllegalStateException.class)
	public void shouldThrowExceptionIfPasswordIsEmpty()
	{
		initUserPasswordService.readUserPassword("user1");
	}

	@Test(expected = IllegalStateException.class)
	public void shouldThrowExceptionIfPasswordIsNotProvidedForUser()
	{
		initUserPasswordService.readUserPassword("user2");
	}

	private PropertiesLoader getMockedPropertiesLoader()
	{
		final PropertiesLoader propertiesLoader = Mockito.mock(PropertiesLoader.class);
		when(propertiesLoader.getProperty("initialpassword.admin")).thenReturn("nimda");
		when(propertiesLoader.getProperty("initialpassword.user")).thenReturn("resu");
		when(propertiesLoader.getProperty("initialpassword.anonymous")).thenReturn("suomynona");
		when(propertiesLoader.getProperty("initialpassword.user1")).thenReturn("");
		return propertiesLoader;
	}
}
