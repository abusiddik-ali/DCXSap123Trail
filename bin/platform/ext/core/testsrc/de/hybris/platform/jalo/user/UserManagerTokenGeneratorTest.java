/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jalo.user;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.core.model.user.EmployeeModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.jalo.c2l.C2LManager;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.util.UUID;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.mock.web.MockHttpServletResponse;
import static org.assertj.core.api.Assertions.assertThat;

import com.google.common.collect.Sets;

@IntegrationTest
public class UserManagerTokenGeneratorTest  extends ServicelayerBaseTest
{

	private final PropertyConfigSwitcher tokenExtendedSwitcher = new PropertyConfigSwitcher("login.token.extended");
	private static final String COOKIE_NAME = "testCookieName";
	private static final String LANG_ISO = "en";

	@Resource
	private UserService userService;

	private UserModel employee;

	@Resource
	private ModelService modelService;

	@Resource
	private TokenService tokenService;


	@Before
	public void setUp()
	{
		employee = createUser();
	}

	@After
	public void clean(){
		tokenExtendedSwitcher.switchBackToDefault();
	}

	@Test
	public void shouldGeneratedBaseTokenBeValid() throws Exception{

		tokenExtendedSwitcher.switchToValue("false");

		final CookieBasedLoginToken cookieFromToken =createCookieBasedLoginBaseOnStoredCookieInResponse(100);

		assertThatBasePartIsNotEmpty(cookieFromToken);
		assertThatBasePartFromTokenIsValid(cookieFromToken);
		assertThatExtendedPartIsEmpty(cookieFromToken);
		assertThat(cookieFromToken.isTokenValid()).isTrue();
	}

	@Test
	public void shouldGeneratedEnhancedTokenBeValid() throws Exception{

		tokenExtendedSwitcher.switchToValue("true");
		final CookieBasedLoginToken cookieFromToken = createCookieBasedLoginBaseOnStoredCookieInResponse(100);
		assertThatBasePartIsNotEmpty(cookieFromToken);
		assertThatBasePartFromTokenIsValid(cookieFromToken);
		assertThatExtendedPartIsNotEmpty(cookieFromToken);
		assertThat(cookieFromToken.isTokenValid()).isTrue();
	}

	@Test
	public void shouldGeneratedEnhancedTokenBeInvalidDueToTokenRevocation() throws Exception{

		tokenExtendedSwitcher.switchToValue("true");
		final CookieBasedLoginToken cookieFromToken = createCookieBasedLoginBaseOnStoredCookieInResponse(100);

		tokenService.revokeTokenForUser(employee.getUid());
		assertThatBasePartIsNotEmpty(cookieFromToken);
		assertThatBasePartFromTokenIsValid(cookieFromToken);
		assertThatExtendedPartIsNotEmpty(cookieFromToken);
		assertThat(cookieFromToken.isTokenValid()).isFalse();
	}

	@Test
	public void shouldGeneratedEnhancedTokenBeValidWithDbTokenPart() throws Exception{

		tokenExtendedSwitcher.switchToValue("true");
		final String token = tokenService.getOrCreateTokenForUser(employee.getUid());
		final CookieBasedLoginToken cookieFromToken = createCookieBasedLoginBaseOnStoredCookieInResponse(100);

		assertThatBasePartIsNotEmpty(cookieFromToken);
		assertThatBasePartFromTokenIsValid(cookieFromToken);
		assertThatExtendedPartIsNotEmpty(cookieFromToken);
		assertThat(cookieFromToken.getTokenDbPart()).isEqualTo(token);
		assertThat(cookieFromToken.isTokenValid()).isTrue();
	}

	@Test
	public void shouldGeneratedEnhancedTokenWithExpiredTTLTimestamp() throws Exception{

		tokenExtendedSwitcher.switchToValue("true");
		final CookieBasedLoginToken cookieFromToken =  createCookieBasedLoginBaseOnStoredCookieInResponse(1);

		Thread.sleep(1000);

		assertThatBasePartIsNotEmpty(cookieFromToken);
		assertThatBasePartFromTokenIsValid(cookieFromToken);
		assertThatExtendedPartIsNotEmpty(cookieFromToken);
		assertThat(cookieFromToken.isTokenValid()).isFalse();
	}


	@Test
	public void shouldNotFailOnTotallyInvalidToken() throws Exception
	{

		tokenExtendedSwitcher.switchToValue("true");
		final String randomToken= UUID.randomUUID().toString();
		final CookieBasedLoginToken cookieFromToken = encryptAndGenerateCookieBasedLogenToken(randomToken);

		assertThatBasePartIsEmpty(cookieFromToken);
		assertThatExtendedPartIsEmpty(cookieFromToken);
		assertThat(cookieFromToken.isTokenValid()).isFalse();
	}

	@Test
	public void shouldNotFailOnPartiallyInvalidToken() throws Exception
	{

		tokenExtendedSwitcher.switchToValue("true");
		final String partiallyInvalidToken = employee.getPk().toString() + "_" + "dummyLangIsoCode"+ "_" + UUID.randomUUID().toString();
		final CookieBasedLoginToken cookieFromToken = encryptAndGenerateCookieBasedLogenToken(partiallyInvalidToken);

		assertThat(cookieFromToken.getUser()).isNotNull();
		final User employeeJalo = modelService.getSource(employee);
		assertThat(UserManager.getInstance().checkPassword(employeeJalo,cookieFromToken)).isFalse();
		assertThatExtendedPartIsEmpty(cookieFromToken);

	}

	@Test
	public void shouldNotFailWhenTTLTimestampIsBroken() throws Exception
	{

		tokenExtendedSwitcher.switchToValue("true");
		final String langString = C2LManager.getInstance().getLanguageByIsoCode(LANG_ISO).getPK().toString();
		final User employeeJalo = modelService.getSource(employee);

		final String password = UserManager.getInstance()
		                  .getEncodedPasswordForLoginCookie(employeeJalo.getUid(), null, employeeJalo);
		final String partiallyInvalidToken = employee.getPk().toString() + "_" + langString + "_" + password + "_" + "invalidTTL" + "_" + UUID.randomUUID();

		final CookieBasedLoginToken cookieFromToken = encryptAndGenerateCookieBasedLogenToken(partiallyInvalidToken);

		assertThatBasePartIsNotEmpty(cookieFromToken);
		assertThat(cookieFromToken.isTokenValid()).isFalse();

	}

	@Test
	public void shouldNotFailWhenTTLTimestampIsBrokenOld() throws Exception
	{
		tokenExtendedSwitcher.switchToValue("false");
		final String langString = C2LManager.getInstance().getLanguageByIsoCode(LANG_ISO).getPK().toString();
		final User employeeJalo = modelService.getSource(employee);

		final String password = UserManager.getInstance()
		                                   .getEncodedPasswordForLoginCookie(employeeJalo.getUid(), null, employeeJalo);
		final String partiallyInvalidToken = employee.getPk().toString() + "_" + langString + "_" + password + "_" + "invalidTTL" + "_" + UUID.randomUUID();

		final CookieBasedLoginToken cookieFromToken = encryptAndGenerateCookieBasedLogenToken(partiallyInvalidToken);

		assertThatBasePartIsNotEmpty(cookieFromToken);
		assertThatBasePartFromTokenIsValid(cookieFromToken);
		assertThat(cookieFromToken.isTokenValid()).isTrue();
	}


	private CookieBasedLoginToken encryptAndGenerateCookieBasedLogenToken(final String tokenValue) throws Exception{
		final String encryptedValue = Registry.getMasterTenant().getValueEncryptor().encrypt(tokenValue);
		return new CookieBasedLoginToken(new Cookie(COOKIE_NAME, encryptedValue));
	}

    private void assertThatBasePartFromTokenIsValid(final CookieBasedLoginToken cookieFromToken)throws Exception{
	    final User employeeJalo = modelService.getSource(employee);
	    assertThat(cookieFromToken.getUser().getPK()).isEqualTo(employeeJalo.getPK());
	    assertThat(cookieFromToken.getLanguage().getIsocode()).isEqualTo(LANG_ISO);
	 //   assertThat(UserManager.getInstance().checkPassword(employeeJalo,cookieFromToken)).isTrue();
	    assertThatPasswordIsValid(cookieFromToken);
    }


	private void assertThatPasswordIsValid(final CookieBasedLoginToken cookieFromToken)throws Exception{
		final User employeeJalo = modelService.getSource(employee);
		assertThat(UserManager.getInstance().checkPassword(employeeJalo,cookieFromToken)).isTrue();
	}

	private void assertThatPasswordIsInvalid(final CookieBasedLoginToken cookieFromToken)throws Exception{
		final User employeeJalo = modelService.getSource(employee);
		assertThat(UserManager.getInstance().checkPassword(employeeJalo,cookieFromToken)).isFalse();
	}


	private void assertThatBasePartIsNotEmpty(final CookieBasedLoginToken cookieFromToken){
		assertThat(cookieFromToken.getUser()).isNotNull();
		assertThat(cookieFromToken.getPassword()).isNotNull();
		assertThat(cookieFromToken.getLanguage()).isNotNull();
	}

	private void assertThatBasePartIsEmpty(final CookieBasedLoginToken cookieFromToken){
		assertThat(cookieFromToken.getUser()).isNull();
		assertThat(cookieFromToken.getPassword()).isNull();
		assertThat(cookieFromToken.getLanguage()).isNull();
	}


	private void assertThatExtendedPartIsNotEmpty(final CookieBasedLoginToken cookieFromToken){
		assertThat(cookieFromToken.getTtlTimestamp()).isNotNull();
		assertThat(cookieFromToken.getPasswordSalt()).isNotNull();
	}

	private void assertThatExtendedPartIsEmpty(final CookieBasedLoginToken cookieFromToken){
		assertThat(cookieFromToken.getTtlTimestamp()).isNull();
		assertThat(cookieFromToken.getPasswordSalt()).isNull();
	}

	private CookieBasedLoginToken createCookieBasedLoginBaseOnStoredCookieInResponse(final int ttl) throws Exception{
		final MockHttpServletResponse response = new MockHttpServletResponse();
		UserManager.getInstance().storeLoginTokenCookie(COOKIE_NAME ,employee.getUid(), LANG_ISO, null,"path","domain",true,ttl,response);
		return new CookieBasedLoginToken(response.getCookie(COOKIE_NAME));
	}

	@Test
	public void shouldTokenBeInvalidButNotFailWhenIsInOldFormatButProceededWithEnhancedTokenGenerator() throws Exception{
		tokenExtendedSwitcher.switchToValue("false");
		final User employeeJalo = modelService.getSource(employee);
		final String oldToken = UserManager.getInstance().doGenerateLoginTokenCookieValue(employee.getUid(),LANG_ISO,null,employeeJalo,100);
		tokenExtendedSwitcher.switchToValue("true");
		final CookieBasedLoginToken cookieBasedLoginToken = encryptAndGenerateCookieBasedLogenToken(oldToken);
		assertThatBasePartIsNotEmpty(cookieBasedLoginToken);
		assertThatExtendedPartIsEmpty(cookieBasedLoginToken);
		assertThatPasswordIsInvalid(cookieBasedLoginToken);
		assertThat(cookieBasedLoginToken.isTokenValid()).isFalse();

	}

	@Test
	public void shouldTokenBeValidButWithInvalidPasswordWhenIsInNewFormatButProceededWithBaseOldTokenGenerator()throws Exception{

		tokenExtendedSwitcher.switchToValue("true");
		final User employeeJalo = modelService.getSource(employee);
		final String oldToken = UserManager.getInstance().doGenerateLoginTokenCookieValue(employee.getUid(),LANG_ISO,null,employeeJalo,100);
		tokenExtendedSwitcher.switchToValue("false");
		final CookieBasedLoginToken cookieBasedLoginToken = encryptAndGenerateCookieBasedLogenToken(oldToken);

		assertThatBasePartIsNotEmpty(cookieBasedLoginToken);
		assertThatExtendedPartIsEmpty(cookieBasedLoginToken);
		assertThatPasswordIsInvalid(cookieBasedLoginToken);
		assertThat(cookieBasedLoginToken.isTokenValid()).isTrue();
	}



	private UserModel createUser(){
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

}
