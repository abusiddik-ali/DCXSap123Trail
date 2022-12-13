/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.security.auth.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.ThrowableAssert.catchThrowable;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.core.model.user.EmployeeModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.jalo.user.DefaultUserPasswordCheckingStrategy;
import de.hybris.platform.jalo.user.Employee;
import de.hybris.platform.jalo.user.StringBasedLoginToken;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.security.auth.AuthenticationService;
import de.hybris.platform.servicelayer.security.auth.InvalidCredentialsException;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.servicelayer.user.impl.DefaulPasswordEncoderService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.util.UUID;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Sets;


@IntegrationTest
public class AuthenticationIntegrationTest extends ServicelayerTransactionalBaseTest
{
	@Resource
	private UserService userService;

	@Resource
	private ModelService modelService;

	@Resource
	private AuthenticationService authenticationService;

	@Resource
	DefaulPasswordEncoderService passwordEncoderService;

	private UserModel employee;

	private final PropertyConfigSwitcher acceptEmpty = new PropertyConfigSwitcher("user.password.acceptEmpty");

	@Before
	public void setUp()
	{
		final PrincipalGroupModel adminGroup = userService.getAdminUserGroup();

		employee = modelService.create(EmployeeModel.class);
		final String value = UUID.randomUUID().toString();
		employee.setUid(value);
		employee.setName(value);
		employee.setGroups(Sets.newHashSet(adminGroup));
		modelService.saveAll();

		acceptEmpty.switchToValue("false");
	}

	@After
	public void clenUp()
	{
		acceptEmpty.switchBackToDefault();
	}

	@Test
	public void logingShouldFailForEmptyPasswordTest()
	{
		final Throwable slLoginException;
		final boolean jaloLogin;

		//when
		jaloLogin = ((Employee) modelService.getSource(employee)).checkPassword(new StringBasedLoginToken(""));
		slLoginException = catchThrowable(() -> authenticationService.login(employee.getName(), ""));


		//then
		assertThat(slLoginException).isInstanceOf(InvalidCredentialsException.class);
		assertThat(jaloLogin).isFalse();
	}

	@Test
	public void encoderShouldNotAcceptEmptyPasswordTest()
	{
		//when
		final boolean valid = passwordEncoderService.isValid(employee, "");

		//then
		assertThat(valid).isFalse();
	}

	@Test
	public void encoderShouldAcceptEmptyPasswordWithAcceptEmptyFlagSetToTrueTest()
	{
		//given
		acceptEmpty.switchToValue("true");

		//when
		final boolean valid = passwordEncoderService.isValid(employee, "");

		//then
		assertThat(valid).isTrue();
	}

	@Test
	public void encoderWithEncodingShouldNotAcceptEmptyPasswordTest()
	{
		//when
		final boolean valid = passwordEncoderService.isValid(employee, "md5", "");

		//then
		assertThat(valid).isFalse();
	}

	@Test
	public void encoderWithEncodingShouldAcceptEmptyPasswordWithAcceptEmptyFlagSetToTrueTest()
	{
		//given
		acceptEmpty.switchToValue("true");

		//when
		final boolean valid = passwordEncoderService.isValid(employee, "md5", "");

		//then
		assertThat(valid).isTrue();
	}

	@Test
	public void defaultUserPasswordCheckingStrategyShouldNotAcceptEmptyPasswordTest()
	{
		//given
		final DefaultUserPasswordCheckingStrategy defaultUserPasswordCheckingStrategy = new DefaultUserPasswordCheckingStrategy();

		//when
		final boolean valid = defaultUserPasswordCheckingStrategy.checkPassword(modelService.getSource(employee), "");

		//then
		assertThat(valid).isFalse();
	}

	@Test
	public void defaultUserPasswordCheckingStrategyShouldAcceptEmptyPasswordWithAcceptEmptyFlagSetToTrueTest()
	{
		//given
		final DefaultUserPasswordCheckingStrategy defaultUserPasswordCheckingStrategy = new DefaultUserPasswordCheckingStrategy();
		acceptEmpty.switchToValue("true");

		//when
		final boolean valid = defaultUserPasswordCheckingStrategy.checkPassword(modelService.getSource(employee), "");

		//then
		assertThat(valid).isTrue();
	}
}
