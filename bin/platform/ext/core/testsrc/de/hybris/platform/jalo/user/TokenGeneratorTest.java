/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jalo.user;

import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.core.model.user.EmployeeModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;

import java.util.UUID;

import javax.annotation.Resource;

import com.google.common.collect.Sets;

public abstract class TokenGeneratorTest extends ServicelayerBaseTest
{
	protected static final String DELIMITER = "_";
	protected UserModel employee;
	@Resource
	private UserService userService;
	@Resource
	private ModelService modelService;


	protected String generateToken(final int ttl, final String lang) throws Exception
	{

		final String uid = employee.getUid();
		final String plainTextPassword = null;
		final User employeeJalo = modelService.getSource(employee);

		final TokenParams paramsForToken = new TokenParams.Builder().withUid(uid)
		                                                            .withLanguageIso(lang)
		                                                            .withPlainTextPassword(plainTextPassword)
		                                                            .withUser(employeeJalo)
		                                                            .withTTL(ttl)
		                                                            .withDelimiter(DELIMITER)
		                                                            .build();
		return TokenGeneratorProvider.getInstance().getTokenGenerator().generateToken(paramsForToken);

	}

	protected UserModel createUser()
	{
		final PrincipalGroupModel adminGroup = userService.getAdminUserGroup();
		final UserModel user = modelService.create(EmployeeModel.class);
		final String value = UUID.randomUUID().toString();
		user.setUid(value);
		user.setName(value);
		user.setGroups(Sets.newHashSet(adminGroup));

		userService.setPassword(user, UUID.randomUUID().toString(),
				"md5");
		modelService.saveAll();
		return user;
	}
}
