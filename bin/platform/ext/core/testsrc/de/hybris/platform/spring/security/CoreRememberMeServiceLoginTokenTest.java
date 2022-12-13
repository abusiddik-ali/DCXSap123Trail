/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.spring.security;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.core.model.user.EmployeeModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.jalo.user.CookieBasedLoginToken;
import de.hybris.platform.jalo.user.LoginToken;
import de.hybris.platform.jalo.user.StringBasedLoginToken;
import de.hybris.platform.jalo.user.UserManager;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.Config;

import java.util.UUID;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.security.core.Authentication;

import com.google.common.collect.Sets;

@IntegrationTest
public class CoreRememberMeServiceLoginTokenTest extends ServicelayerBaseTest
{
	private static final String LANG_ISO = "en";
	private final PropertyConfigSwitcher tokenExtendedSwitcher = new PropertyConfigSwitcher("login.token.extended");
	private final PropertyConfigSwitcher autoLoginHacEnabled = new PropertyConfigSwitcher("login.token.authentication.hac.enabled");
	private final PropertyConfigSwitcher autoLoginEnabled = new PropertyConfigSwitcher("login.token.authentication.enabled");
	private final PropertyConfigSwitcher tokenUrlEnabled = new PropertyConfigSwitcher ("login.token.url.enabled");

	@Resource
	private UserService userService;
	private UserModel employee;
	@Resource
	private ModelService modelService;
	@Spy
	private CoreRememberMeService rememberMeService;

	@Before
	public void setUp()
	{
		MockitoAnnotations.initMocks(this);
		doReturn(new CoreUserDetailsService()).when(rememberMeService).lookupUserDetailsService();
		rememberMeService.setCookieName(Config.getParameter(CookieBasedLoginToken.NAME_KEY));
		employee = createUser();
	}

	@After
	public void clean()
	{
		tokenExtendedSwitcher.switchBackToDefault();
		autoLoginHacEnabled.switchBackToDefault();
		autoLoginEnabled.switchBackToDefault();
		tokenUrlEnabled.switchBackToDefault();
	}




	@Test
	public void processAutoLoginWithEnhancedTokenShouldBeSuccessful() throws Exception
	{
		//given
		tokenExtendedSwitcher.switchToValue("true");
		final MockHttpServletRequest request = new MockHttpServletRequest();
		final MockHttpServletResponse response = new MockHttpServletResponse();
		final Cookie cookieBasedLoginToken = createCookieBasedLoginBaseOnStoredCookieInResponse(100);
		request.setCookies(cookieBasedLoginToken);
		//when
		final Authentication result = rememberMeService.autoLogin(request, response);

		//then
		assertThat(result).isNotNull();
	}


	@Test
	public void processAutoLoginWithEnhancedTokenWithExpiredTTLShouldReturnNullAuthentication() throws Exception
	{
		//given
		tokenExtendedSwitcher.switchToValue("true");
		final MockHttpServletRequest request = new MockHttpServletRequest();
		final MockHttpServletResponse response = new MockHttpServletResponse();
		final Cookie cookieBasedLoginToken = createCookieBasedLoginBaseOnStoredCookieInResponse(1);
		request.setCookies(cookieBasedLoginToken);

		//when
		Thread.sleep(1100);
		final Authentication result = rememberMeService.autoLogin(request, response);

		//then
		assertThat(result).isNull();
	}

	@Test
	public void processAutoLoginWithBaseTokenWithExpiredTTLShouldBeSuccessful() throws Exception
	{
		//given
		tokenExtendedSwitcher.switchToValue("false");
		final MockHttpServletRequest request = new MockHttpServletRequest();
		final MockHttpServletResponse response = new MockHttpServletResponse();
		final Cookie cookieBasedLoginToken = createCookieBasedLoginBaseOnStoredCookieInResponse(1);
		request.setCookies(cookieBasedLoginToken);

		//when
		Thread.sleep(1100);
		final Authentication result = rememberMeService.autoLogin(request, response);

		//then
		assertThat(result).isNotNull();
	}

	@Test
	public void processAutoLoginWithBaseTokenStrategyShouldReturnAuthenticationForEnhancedToken() throws Exception
	{
		//NOTE password is not verified in autoLogin
		//given
		tokenExtendedSwitcher.switchToValue("true");
		final MockHttpServletRequest request = new MockHttpServletRequest();
		final MockHttpServletResponse response = new MockHttpServletResponse();
		final Cookie cookieBasedLoginToken = createCookieBasedLoginBaseOnStoredCookieInResponse(100);
		request.setCookies(cookieBasedLoginToken);

		//when
		tokenExtendedSwitcher.switchToValue("false");
		final Authentication result = rememberMeService.autoLogin(request, response);

		//then
		assertThat(result).isNotNull();
	}

	@Test
	public void processAutoLoginWithEnhancedTokenStrategyShouldReturnEmptyAuthenticationForBaseToken() throws Exception
	{
		//NOTE password is not verified in autoLogin
		//given
		tokenExtendedSwitcher.switchToValue("false");
		final MockHttpServletRequest request = new MockHttpServletRequest();
		final MockHttpServletResponse response = new MockHttpServletResponse();
		final Cookie cookieBasedLoginToken = createCookieBasedLoginBaseOnStoredCookieInResponse(100);
		request.setCookies(cookieBasedLoginToken);

		//when
		tokenExtendedSwitcher.switchToValue("true");
		final Authentication result = rememberMeService.autoLogin(request, response);

		//then
		assertThat(result).isNull();
	}


	@Test
	public void processAutoLoginWithBaseTokenShouldBeSuccessful() throws Exception
	{
		//given
		tokenExtendedSwitcher.switchToValue("false");
		final MockHttpServletRequest request = new MockHttpServletRequest();
		final MockHttpServletResponse response = new MockHttpServletResponse();
		final Cookie cookieBasedLoginToken = createCookieBasedLoginBaseOnStoredCookieInResponse(100);
		request.setCookies(cookieBasedLoginToken);

		//when
		final Authentication result = rememberMeService.autoLogin(request, response);

		//then
		assertThat(result).isNotNull();
	}

	@Test
	public void processAutoLoginShouldReturnNullWhenAutoLoginIsDisabled() throws Exception
	{
		//given
		tokenExtendedSwitcher.switchToValue("true");
		autoLoginEnabled.switchToValue("false");

		final MockHttpServletRequest request = new MockHttpServletRequest();
		final MockHttpServletResponse response = new MockHttpServletResponse();
		final Cookie cookieBasedLoginToken = createCookieBasedLoginBaseOnStoredCookieInResponse(100);
		request.setCookies(cookieBasedLoginToken);
		//when
		final Authentication result = rememberMeService.autoLogin(request, response);

		//then
		assertThat(result).isNull();
	}

	@Test
	public void processAutoLoginShouldReturnNullWhenFunctionalityOfLoginTokenIsDisabledForHac() throws Exception
	{
		//given
		tokenExtendedSwitcher.switchToValue("true");
		autoLoginHacEnabled.switchToValue("false");
		autoLoginEnabled.switchToValue("true");

		final MockHttpServletRequest request = new MockHttpServletRequest();
		final MockHttpServletResponse response = new MockHttpServletResponse();
		final Cookie cookieBasedLoginToken = createCookieBasedLoginBaseOnStoredCookieInResponse(100);
		request.setCookies(cookieBasedLoginToken);
		//when
		final Authentication result = rememberMeService.autoLogin(request, response);

		//then
		assertThat(result).isNull();
	}

	@Test
	public void shouldNotProceedAutoLoginForStringBasedLoginTokenWhenPropertyDisabled()
	{
		disableUrlLoginToken();

		final MockHttpServletRequest request = new MockHttpServletRequest();
		associateStringBasedLoginTokenToRequest(request);
		final MockHttpServletResponse response = new MockHttpServletResponse();
		rememberMeService.autoLogin(request, response);

		verify(rememberMeService, never()).processAutoLoginCookie(anyObject(), anyObject(), anyObject());
	}

	@Test
	public void shouldProceedAutoLoginForStringBasedLoginTokenWhenPropertyEnabled()
	{
		enableUrlLoginToken();

		final MockHttpServletRequest request = new MockHttpServletRequest();
		associateStringBasedLoginTokenToRequest(request);
		final MockHttpServletResponse response = new MockHttpServletResponse();
		rememberMeService.autoLogin(request, response);

		verify(rememberMeService, times(1)).processAutoLoginCookie(anyObject(), anyObject(), anyObject());
	}

	private Cookie createCookieBasedLoginBaseOnStoredCookieInResponse(final int ttl) throws Exception
	{
		final MockHttpServletResponse response = new MockHttpServletResponse();
		UserManager.getInstance()
		           .storeLoginTokenCookie(Config.getParameter(CookieBasedLoginToken.NAME_KEY), employee.getUid(), LANG_ISO, null,
				           "path", "domain", true, ttl, response);
		return response.getCookie(Config.getParameter(CookieBasedLoginToken.NAME_KEY));
	}

	private UserModel createUser()
	{
		final PrincipalGroupModel adminGroup = userService.getAdminUserGroup();
		final UserModel employee = modelService.create(EmployeeModel.class);
		final String value = UUID.randomUUID().toString();
		employee.setUid(value);
		employee.setName(value);
		employee.setGroups(Sets.newHashSet(adminGroup));

		userService.setPassword(employee, UUID.randomUUID().toString(),
				"md5");
		modelService.saveAll();
		return employee;
	}

	private void associateStringBasedLoginTokenToRequest(final MockHttpServletRequest request)
	{
		request.setParameter(Config.getParameter(LoginToken.URL_PARAMETER_KEY), createStringBasedLoginToken().toString());
	}

	private void disableUrlLoginToken()
	{
		tokenUrlEnabled.switchToValue("false");
	}

	private void enableUrlLoginToken()
	{
		tokenUrlEnabled.switchToValue("true");
	}

	private StringBasedLoginToken createStringBasedLoginToken()
	{
		return new StringBasedLoginToken("value");
	}
}
