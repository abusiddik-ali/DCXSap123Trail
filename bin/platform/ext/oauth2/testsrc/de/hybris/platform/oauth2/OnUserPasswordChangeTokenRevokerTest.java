/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.oauth2;

import static de.hybris.platform.oauth2.OnUserPasswordChangeTokenRevoker.ENABLED_CONFIG_KEY;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.webservicescommons.model.OAuthAccessTokenModel;
import de.hybris.platform.webservicescommons.model.OAuthClientDetailsModel;
import de.hybris.platform.webservicescommons.model.OAuthRefreshTokenModel;
import de.hybris.platform.webservicescommons.oauth2.token.OAuthTokenService;

import java.util.List;
import java.util.UUID;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class OnUserPasswordChangeTokenRevokerTest extends ServicelayerBaseTest
{
	@Resource
	UserService userService;

	@Resource
	ModelService modelService;

	@Resource
	OAuthTokenService oauthTokenService;

	private String testCustomerId;
	private final PropertyConfigSwitcher enabledFlag = new PropertyConfigSwitcher(ENABLED_CONFIG_KEY);

	@Before
	public void prepareTestData()
	{
		final OAuthClientDetailsModel client = modelService.create(OAuthClientDetailsModel.class);
		client.setClientId(uniqueString());

		final CustomerModel customer = modelService.create(CustomerModel.class);
		customer.setUid(uniqueString());
		customer.setPassword("TEST");

		modelService.saveAll();

		final OAuthRefreshTokenModel refreshToken = modelService.create(OAuthRefreshTokenModel.class);
		refreshToken.setTokenId(uniqueString());
		refreshToken.setToken(uniqueString());
		refreshToken.setAuthentication(uniqueString());

		final OAuthAccessTokenModel accessToken = modelService.create(OAuthAccessTokenModel.class);
		accessToken.setTokenId(uniqueString());
		accessToken.setToken(uniqueString());
		accessToken.setAuthenticationId(uniqueString());
		accessToken.setAuthentication(uniqueString());
		accessToken.setClient(client);
		accessToken.setUser(customer);
		accessToken.setRefreshToken(refreshToken);

		oauthTokenService.saveAccessToken(accessToken);

		testCustomerId = customer.getUid();
		modelService.detachAll();
	}

	@After
	public void restoreEnabledFlag()
	{
		enabledFlag.switchBackToDefault();
	}

	private String uniqueString()
	{
		return UUID.randomUUID().toString();
	}

	@Test
	public void shouldRevokeTokensWhenPasswordChanged()
	{
		enabledFlag.switchToValue("true");
		assertThat(oauthTokenService.getAccessTokensForUser(testCustomerId)).isNotEmpty();

		userService.setPassword(testCustomerId, "NEW");

		assertThat(oauthTokenService.getAccessTokensForUser(testCustomerId)).isEmpty();
	}

	@Test
	public void shouldNotRevokeTokensWhenPasswordChangedAndRevokerIsDisabled()
	{
		enabledFlag.switchToValue("false");
		final List<OAuthAccessTokenModel> tokensBefore = oauthTokenService.getAccessTokensForUser(testCustomerId);
		assertThat(tokensBefore).isNotEmpty();

		userService.setPassword(testCustomerId, "NEW");

		assertThat(oauthTokenService.getAccessTokensForUser(testCustomerId)).isNotEmpty().isEqualTo(tokensBefore);
	}
}