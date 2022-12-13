/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.interceptor;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.core.model.user.EmployeeModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.impex.ImportConfig;
import de.hybris.platform.servicelayer.impex.ImportResult;
import de.hybris.platform.servicelayer.impex.ImportService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.type.TypeService;
import de.hybris.platform.servicelayer.user.daos.UserDao;

import java.util.UUID;

import javax.annotation.Resource;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@IntegrationTest
public class DisableLoginForImportedUserInterceptorIntegrationTest extends ServicelayerBaseTest
{
	private static final Logger LOGGER = LoggerFactory.getLogger(DisableLoginForImportedUserInterceptorIntegrationTest.class);
	private static final int NUMBER = 0;
	private static final int USER_PASSWORD = 1;
	private static final int USER_LOGIN_DISABLED = 2;
	private static final int IMPEX_PASSWORD = 3;
	private static final int IMPEX_LOGIN_DISABLED = 4;
	private static final int EXPECTED_LOGIN_DISABLED = 5;
	private static final Object[][] DECISION_TABLE = new Object[][]{
			testCase(1, null, null, null, null, true),
			testCase(2, null, null, null, false, true),
			testCase(3, null, null, null, true, true),
			testCase(4, null, null, "", null, true),
			testCase(5, null, null, "", false, true),
			testCase(6, null, null, "", true, true),
			testCase(7, null, null, "abcd", null, false),
			testCase(8, null, null, "abcd", false, false),
			testCase(9, null, null, "abcd", true, true),
			testCase(10, "", false, null, null, true),
			testCase(11, "", false, null, false, true),
			testCase(12, "", false, null, true, true),
			testCase(13, "", false, "", null, true),
			testCase(14, "", false, "", false, true),
			testCase(15, "", false, "", true, true),
			testCase(16, "", false, "abcd", null, false),
			testCase(17, "", false, "abcd", false, false),
			testCase(18, "", false, "abcd", true, true),
			testCase(19, "", true, null, null, true),
			testCase(20, "", true, null, false, true),
			testCase(21, "", true, null, true, true),
			testCase(22, "", true, "", null, true),
			testCase(23, "", true, "", false, true),
			testCase(24, "", true, "", true, true),
			testCase(25, "", true, "abcd", null, true),
			testCase(26, "", true, "abcd", false, false),
			testCase(27, "", true, "abcd", true, true),
			testCase(28, "1234", false, null, null, false),
			testCase(29, "1234", false, null, false, false),
			testCase(30, "1234", false, null, true, true),
			testCase(31, "1234", false, "", null, true),
			testCase(32, "1234", false, "", false, true),
			testCase(33, "1234", false, "", true, true),
			testCase(34, "1234", false, "abcd", null, false),
			testCase(35, "1234", false, "abcd", false, false),
			testCase(36, "1234", false, "abcd", true, true),
			testCase(37, "1234", true, null, null, true),
			testCase(38, "1234", true, null, false, false),
			testCase(39, "1234", true, null, true, true),
			testCase(40, "1234", true, "", null, true),
			testCase(41, "1234", true, "", false, true),
			testCase(42, "1234", true, "", true, true),
			testCase(43, "1234", true, "abcd", null, true),
			testCase(44, "1234", true, "abcd", false, false),
			testCase(45, "1234", true, "abcd", true, true)
	};

	@Resource
	ImportService importService;

	@Resource
	ModelService modelService;

	@Resource
	UserDao userDao;

	@Resource
	TypeService typeService;

	@Resource
	DisableLoginForImportedUserInterceptor disableLoginForImportedUserInterceptor;

	private static Object[] testCase(final int number, final String userPassword, final Boolean userLoginDisabled,
	                                 final String impexPassword, final Boolean impexLoginDisabled,
	                                 final boolean expectedLoginDisabled)
	{
		return new Object[]{ number, userPassword, userLoginDisabled, impexPassword, impexLoginDisabled, expectedLoginDisabled };
	}

	@Test
	public void shouldNotAffectModelService()
	{
		disableLoginForImportedUserInterceptor.forceEnabled(true);
		try
		{
			final String uid = givenUniqueUID();

			final UserModel user = modelService.create(UserModel.class);
			user.setUid(uid);
			user.setName(uniqueName());
			user.setLoginDisabled(false);
			modelService.save(user);
			modelService.detachAll();

			final UserModel createdUser = userDao.findUserByUID(uid);
			assertThat(createdUser).isNotNull();
			assertThat(createdUser.isLoginDisabled()).isFalse();
		}
		finally
		{
			disableLoginForImportedUserInterceptor.forceEnabled(null);
		}
	}

	@Test
	public void shouldFollowDecisionTableForEmployeeImportedByImpExNoDistributedImpEx()
	{
		assertThatAllImpExRelevantTestCasesSucceeded(EmployeeModel.class, false);
	}

	@Test
	public void shouldFollowDecisionTableForUserImportedByImpExNoDistributedImpEx()
	{
		assertThatAllImpExRelevantTestCasesSucceeded(UserModel.class, false);
	}

	@Test
	public void shouldFollowDecisionTableForCustomerImportedByImpExNoDistributedImpEx()
	{
		assertThatAllImpExRelevantTestCasesSucceeded(CustomerModel.class, false);
	}

	@Test
	public void shouldFollowDecisionTableForEmployeeImportedByUserRightsNoDistributedImpEx()
	{
		assertThatAllUserRightsRelevantTestCasesSucceeded(EmployeeModel.class, false);
	}

	@Test
	public void shouldFollowDecisionTableForUserImportedByUserRightsNoDistributedImpEx()
	{
		assertThatAllUserRightsRelevantTestCasesSucceeded(UserModel.class, false);
	}

	@Test
	public void shouldFollowDecisionTableForCustomerImportedByUserRightsNoDistributedImpEx()
	{
		assertThatAllUserRightsRelevantTestCasesSucceeded(CustomerModel.class, false);
	}

	@Test
	public void shouldFollowDecisionTableForEmployeeImportedByImpExDistributedImpEx()
	{
		assertThatAllImpExRelevantTestCasesSucceeded(EmployeeModel.class, true);
	}

	@Test
	public void shouldFollowDecisionTableForUserImportedByImpExDistributedImpEx()
	{
		assertThatAllImpExRelevantTestCasesSucceeded(UserModel.class, true);
	}

	@Test
	public void shouldFollowDecisionTableForCustomerImportedByImpExDistributedImpEx()
	{
		assertThatAllImpExRelevantTestCasesSucceeded(CustomerModel.class, true);
	}

	@Test
	public void shouldFollowDecisionTableForEmployeeImportedByUserRightsDistributedImpEx()
	{
		assertThatAllUserRightsRelevantTestCasesSucceeded(EmployeeModel.class, true);
	}

	@Test
	public void shouldFollowDecisionTableForUserImportedByUserRightsDistributedImpEx()
	{
		assertThatAllUserRightsRelevantTestCasesSucceeded(UserModel.class, true);
	}

	@Test
	public void shouldFollowDecisionTableForCustomerImportedByUserRightsDistributedImpEx()
	{
		assertThatAllUserRightsRelevantTestCasesSucceeded(CustomerModel.class, true);
	}

	private void assertThatAllImpExRelevantTestCasesSucceeded(final Class<? extends UserModel> userClass,
	                                                          final boolean distributedImpEx)
	{
		for (final Object[] testCase : DECISION_TABLE)
		{
			assertThatTestCaseSucceeded(userClass, testCase, false, distributedImpEx);
		}
	}

	private void assertThatAllUserRightsRelevantTestCasesSucceeded(final Class<? extends UserModel> userClass,
	                                                               final boolean distributedImpEx)
	{
		for (final Object[] testCase : DECISION_TABLE)
		{
			if (testCase[IMPEX_LOGIN_DISABLED] != null)
			{
				continue;
			}
			assertThatTestCaseSucceeded(userClass, testCase, true, distributedImpEx);
		}
	}

	private void assertThatTestCaseSucceeded(final Class<? extends UserModel> userClass, final Object[] testCase,
	                                         final boolean useUserRights, final boolean distributedImpEx)
	{
		disableLoginForImportedUserInterceptor.forceEnabled(true);
		try
		{
			final int testCaseNumber = (int) testCase[NUMBER];
			final boolean expectedLoginDisabled = (boolean) testCase[EXPECTED_LOGIN_DISABLED];

			final String uid = givenUniqueUID();
			givenUser(userClass, testCase, uid);
			final ImportConfig importConfig = givenImpExImport(userClass, testCase, uid, useUserRights, distributedImpEx);

			final ImportResult importResult = importService.importData(importConfig);

			assertThat(importResult)
					.withFailMessage("Test case %d failed. Expecting importResult not to be null.", testCaseNumber)
					.isNotNull();
			assertThat(importResult.isFinished())
					.withFailMessage("Test case %d failed. Expecting importResult to be finished.", testCaseNumber)
					.isTrue();
			assertThat(importResult.isSuccessful())
					.withFailMessage("Test case %d failed. Expecting importResult to be successful.", testCaseNumber)
					.isTrue();

			modelService.detachAll();
			final UserModel user = userDao.findUserByUID(uid);

			assertThat(user)
					.withFailMessage("Test case %d failed. Expecting user not to be null.", testCaseNumber)
					.isNotNull();
			assertThat(user.isLoginDisabled())
					.withFailMessage("Test case %d failed. Expected loginDisabled to be '%s' but was '%s'.", testCaseNumber,
							expectedLoginDisabled, user.isLoginDisabled())
					.isEqualTo(expectedLoginDisabled);
		}
		finally
		{
			disableLoginForImportedUserInterceptor.forceEnabled(null);
		}
	}

	private ImportConfig givenImpExImport(final Class<? extends UserModel> userClass, final Object[] testCase, final String uid,
	                                      final boolean useUserRights, final boolean distributedImpEx)
	{
		final String script = useUserRights ? givenUserRightsScript(userClass, testCase, uid) : givenImpExScript(userClass,
				testCase, uid);
		final ImportConfig config = new ImportConfig();

		config.setEnableCodeExecution(Boolean.TRUE);
		config.setDistributedImpexEnabled(distributedImpEx);
		config.setLegacyMode(false);
		config.setScript(script);

		LOGGER.info("Test case {}.\n{}", testCase[NUMBER], script);

		return config;
	}

	private String givenUserRightsScript(final Class<? extends UserModel> userClass, final Object[] testCase, final String uid)
	{
		final ComposedTypeModel userType = typeService.getComposedTypeForClass(userClass);
		final StringBuilder header = new StringBuilder("$START_USERRIGHTS").append("\nType;UID;MemberOfGroups");
		final StringBuilder data = new StringBuilder(userType.getCode()).append(";").append(uid).append(";");

		final String impexPassword = (String) testCase[IMPEX_PASSWORD];

		if (impexPassword != null)
		{
			header.append(";Password");
			data.append(";").append(impexPassword);
		}

		header.append(";Target;read;change;create;remove;change_perm");
		data.append(";;;;;;");

		return header.append("\n").append(data.toString()).append("\n").append("$END_USERRIGHTS").toString();
	}

	private String givenImpExScript(final Class<? extends UserModel> userClass, final Object[] testCase, final String uid)
	{
		final ComposedTypeModel userType = typeService.getComposedTypeForClass(userClass);
		final StringBuilder header = new StringBuilder("INSERT_UPDATE ").append(userType.getCode()).append(";uid[unique=true]");
		final StringBuilder data = new StringBuilder(";").append(uid);

		final String impexPassword = (String) testCase[IMPEX_PASSWORD];
		final Boolean impexLoginDisabled = (Boolean) testCase[IMPEX_LOGIN_DISABLED];

		if (impexPassword != null)
		{
			header.append(";password");
			data.append(";\"").append(impexPassword).append("\"");
		}

		if (impexLoginDisabled != null)
		{
			header.append(";loginDisabled");
			data.append(";").append(impexLoginDisabled.booleanValue());
		}

		header.append(";name");
		data.append(";").append(uniqueName());

		return header.append("\n").append(data).toString();
	}

	private void givenUser(final Class<? extends UserModel> userClass, final Object[] testCase, final String uid)
	{
		if (testCase[USER_LOGIN_DISABLED] == null)
		{
			return;
		}

		final UserModel user = modelService.create(userClass);

		user.setUid(uid);
		user.setPassword((String) testCase[USER_PASSWORD]);
		user.setLoginDisabled((Boolean) testCase[USER_LOGIN_DISABLED]);
		user.setName(uniqueName());

		modelService.save(user);
	}

	private String uniqueName()
	{
		return "NAME_" + UUID.randomUUID().toString();
	}

	private String givenUniqueUID()
	{
		return ("UID_" + UUID.randomUUID().toString()).toLowerCase();
	}
}
