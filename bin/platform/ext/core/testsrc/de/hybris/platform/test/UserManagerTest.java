/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test;

import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.fail;
import static org.junit.Assert.assertNull;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Constants;
import de.hybris.platform.jalo.ConsistencyCheckException;
import de.hybris.platform.jalo.JaloInvalidParameterException;
import de.hybris.platform.jalo.JaloItemNotFoundException;
import de.hybris.platform.jalo.user.Customer;
import de.hybris.platform.jalo.user.Employee;
import de.hybris.platform.jalo.user.LoginToken;
import de.hybris.platform.jalo.user.StringBasedLoginToken;
import de.hybris.platform.jalo.user.User;
import de.hybris.platform.jalo.user.UserGroup;
import de.hybris.platform.jalo.user.UserManager;
import de.hybris.platform.testframework.HybrisJUnit4TransactionalTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.Config;

import java.util.Collection;

import org.apache.log4j.Logger;
import org.assertj.core.api.Assertions;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.mock.web.MockHttpServletRequest;


@IntegrationTest
public class UserManagerTest extends HybrisJUnit4TransactionalTest
{

	private static final Logger LOG = Logger.getLogger(UserManagerTest.class);
	private UserManager userManager;
	private User user1, user2;

	private static final String URL_BASED_LOGIN_TOKEN_ENABLED = "login.token.url.enabled";
	private final PropertyConfigSwitcher urlTokenEnabled = new PropertyConfigSwitcher (URL_BASED_LOGIN_TOKEN_ENABLED);
	@Before
	public void setUp() throws Exception
	{
		userManager = jaloSession.getUserManager();
		user1 = userManager.createUser("user1");
		assertNotNull(user1);
		user2 = userManager.createUser("user2");
		assertNotNull(user2);
	}

	@After
	public void tearDown()
	{
		urlTokenEnabled.switchBackToDefault ();
	}

	@Test
	public void testIfAnonymousCustomerCanBeDeleted()
	{
		Customer anonymous = null;
		try
		{
			anonymous = userManager.getCustomerByLogin(Constants.USER.ANONYMOUS_CUSTOMER);
			anonymous.remove();
			fail("anonymous customer should not be removable");
		}
		catch (final ConsistencyCheckException e)
		{
			// fine 
		}
		catch (final JaloItemNotFoundException e)
		{
			fail("missing anonymous customer");
		}
		try
		{
			anonymous.setLogin("nerverAllowedForAnonymous");
			fail("anonymous customer UID should not be changeable");
		}
		catch (final ConsistencyCheckException e)
		{
			// fine 
		}
	}

	@Test
	public void testIfAdminCanBeDeleted() throws JaloItemNotFoundException
	{
		Employee admin = null;
		try
		{
			admin = userManager.getAdminEmployee();
			admin.remove();
			fail("admin should not be removable");
		}
		catch (final ConsistencyCheckException e)
		{
			// fine 
		}
		try
		{
			admin.setLogin("nerverAllowedforAdmin");
			fail("admin UID should not be changeable");
		}
		catch (final ConsistencyCheckException e)
		{
			// fine 
		}
	}

	@Test
	public void testIfAdminGroupCanBeDeleted() throws JaloItemNotFoundException
	{
		UserGroup adminGroup = null;
		try
		{
			adminGroup = userManager.getAdminUserGroup();
			adminGroup.remove();
			fail("admin group should not be removable");
		}
		catch (final ConsistencyCheckException e)
		{
			// fine 
		}
		try
		{
			adminGroup.setUID("nerverAllowedForAdminGroup");
			fail("admin group UID should not be changeable");
		}
		catch (final ConsistencyCheckException e)
		{
			// fine 
		}
	}

	@Test
	public void testCreation() throws Exception
	{
		final User customer = userManager.createCustomer("customerLogin");
		customer.remove();
	}

	@Test
	public void testGetAllUsers() throws Exception
	{
		final Collection<? extends User> coll = userManager.getAllUsers();

		// In this test only 2 users are created. But in the system there are also: admin and anoymous, 
		// there could be more users created by other extensions - lets check if the ones we know about are here
		Assertions.assertThat(coll).extracting(User::getUid).contains("admin", "anonymous", "user1", "user2");
	}

	@Test
	public void testGetUserByEmptyLogin() throws Exception
	{
		User customer = userManager.createCustomer("customerLogin");
		try
		{
			customer = userManager.getCustomerByLogin(null);
			fail("Should be impossible to get customer with login null.");
		}
		catch (final NullPointerException e)
		{
			//fine here			
		}
		customer.remove();
	}

	@Test
	public void testCreationWithEmptyUID() throws Exception
	{
		User customer = null;
		try
		{
			customer = userManager.createCustomer(null);
			fail("Should be impossible to set uid to null.");
		}
		catch (final JaloInvalidParameterException e)
		{
			//fine here
			LOG.debug("Expected exception: " + e.getMessage());
		}
		finally
		{
			if (customer != null)
			{
				// wrong end: customer was created :-(
				try
				{
					customer.remove();
					fail("Customer was created!");
				}
				catch (final Exception e)
				{
					// what the ...
				}
			}
		}
	}

	@Test
	public void shouldGetURLBasedLoginTokenWhenPropertyIsDisabled()
	{
		urlTokenEnabled.switchToValue("false");
		final StringBasedLoginToken urlBasedLoginToken = userManager.getURLBasedLoginToken(prepareURLBasedLoginTokenRequest());
		assertNull(urlBasedLoginToken);
	}

	@Test
	public void shouldGetURLBasedLoginTokenWhenPropertyIsEnabled()
	{
		urlTokenEnabled.switchToValue("true");
		final StringBasedLoginToken urlBasedLoginToken = userManager.getURLBasedLoginToken(prepareURLBasedLoginTokenRequest());
		assertNotNull(urlBasedLoginToken);
	}

	private MockHttpServletRequest prepareURLBasedLoginTokenRequest()
	{
		final MockHttpServletRequest httpServletRequest = new MockHttpServletRequest();
		httpServletRequest.setParameter(Config.getParameter(LoginToken.URL_PARAMETER_KEY), "someValue");
		return httpServletRequest;
	}
}
