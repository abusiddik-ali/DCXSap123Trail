/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jalo.user;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.core.model.user.EmployeeModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;

import java.util.UUID;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Sets;


@IntegrationTest
public class TokenServiceTest extends ServicelayerBaseTest
{
	@Resource
	private UserService userService;
	@Resource
	private TokenService tokenService;
	@Resource
	private ModelService modelService;

	protected UserModel employee1;
	protected UserModel employee2;

	@Before
	public void setUp()
	{
		employee1 = createUser();
		employee2 = createUser();
	}

	@Test
	public void shouldReturnTheSameTokenForUser()
	{
		final String getTokenFirstTime = tokenService.getOrCreateTokenForUser(employee1.getUid());
		final String getTokenFirstTimeForSecondUser = tokenService.getOrCreateTokenForUser(employee2.getUid());
		final String getTokenSecondTime = tokenService.getOrCreateTokenForUser(employee1.getUid());
		final String getTokenSecondTimeForSecondUser = tokenService.getOrCreateTokenForUser(employee2.getUid());

		assertThat(getTokenFirstTime).isEqualTo(getTokenSecondTime);
		assertThat(getTokenFirstTimeForSecondUser).isEqualTo(getTokenSecondTimeForSecondUser);
	}

	@Test
	public void shouldReturnTokenBeValid()
	{
		final String getTokenForUser = tokenService.getOrCreateTokenForUser(employee1.getUid());
		assertThat(tokenService.checkIfTokenIsValid(employee1.getUid(), getTokenForUser)).isTrue();
	}

	@Test
	public void shouldReturnTokenBeInvalidAfterTokenRevocation()
	{
		final String getTokenForUser = tokenService.getOrCreateTokenForUser(employee1.getUid());
		final String getTokenForSecondUser = tokenService.getOrCreateTokenForUser(employee2.getUid());
		assertThat(tokenService.checkIfTokenIsValid(employee1.getUid(), getTokenForUser)).isTrue();
		tokenService.revokeTokenForUser(employee1.getUid());
		assertThat(tokenService.checkIfTokenIsValid(employee1.getUid(), getTokenForUser)).isFalse();
		assertThat(tokenService.checkIfTokenIsValid(employee2.getUid(), getTokenForSecondUser)).isTrue();
	}

	@Test
	public void testWhatShouldHappenWhenTokenServiceIsNotWorkingProperly()
	{
		tokenService.getOrCreateTokenForUser(employee1.getUid());
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

}
