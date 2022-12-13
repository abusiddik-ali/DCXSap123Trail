/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.webservicescommons.oauth2.token.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.webservicescommons.model.OAuthClientDetailsModel;
import de.hybris.platform.webservicescommons.oauth2.token.OAuthRevokeTokenService;

import java.util.Collections;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;
import org.springframework.security.oauth2.common.DefaultOAuth2AccessToken;
import org.springframework.security.oauth2.common.DefaultOAuth2RefreshToken;
import org.springframework.security.oauth2.provider.OAuth2Authentication;
import org.springframework.security.oauth2.provider.OAuth2Request;
import org.springframework.security.oauth2.provider.token.TokenStore;

@IntegrationTest
public class DefaultOAuthRevokeTokenServiceTest extends ServicelayerBaseTest
{
	private static final String ACCESS_TOKEN_KEY = "accessToken";
	private static final String REFRESH_TOKEN_KEY = "refreshToken";
	private static final String CLIENT_ID = "clientId";

	@Resource
	private OAuthRevokeTokenService oauthRevokeTokenService;
	@Resource
	private TokenStore oauthTokenStore;
	@Resource
	private ModelService modelService;

	@Before
	public void setUp()
	{
		final OAuthClientDetailsModel client = modelService.create(OAuthClientDetailsModel.class);
		client.setClientId(CLIENT_ID);
		modelService.saveAll(client);

		final DefaultOAuth2AccessToken accessToken = new DefaultOAuth2AccessToken(ACCESS_TOKEN_KEY);
		accessToken.setRefreshToken(new DefaultOAuth2RefreshToken(REFRESH_TOKEN_KEY));
		final OAuth2Request request = new OAuth2Request(Collections.emptyMap(), CLIENT_ID, null, true, Collections.emptySet(),
				null, "/", null, null);
		final OAuth2Authentication auth = new OAuth2Authentication(request, null);
		oauthTokenStore.storeAccessToken(accessToken, auth);
	}

	@Test
	public void revokeAccessTokenShouldRevokeBothTokens()
	{
		oauthRevokeTokenService.revokeAccessToken(ACCESS_TOKEN_KEY);
		assertThat(oauthTokenStore.readRefreshToken(REFRESH_TOKEN_KEY)).isNull();
		assertThat(oauthTokenStore.readAccessToken(ACCESS_TOKEN_KEY)).isNull();
	}

	@Test
	public void revokeRefreshTokenShouldRevokeBothTokens()
	{
		oauthRevokeTokenService.revokeRefreshToken(REFRESH_TOKEN_KEY);
		assertThat(oauthTokenStore.readRefreshToken(REFRESH_TOKEN_KEY)).isNull();
		assertThat(oauthTokenStore.readAccessToken(ACCESS_TOKEN_KEY)).isNull();
	}

	@Test
	public void revokeRefreshTokenShouldNotRevokeAccessToken()
	{
		oauthRevokeTokenService.revokeRefreshToken(ACCESS_TOKEN_KEY);
		assertThat(oauthTokenStore.readRefreshToken(REFRESH_TOKEN_KEY)).isNotNull();
		assertThat(oauthTokenStore.readAccessToken(ACCESS_TOKEN_KEY)).isNotNull();
	}

	@Test
	public void revokeAccessTokenShouldNotRevokeRefreshToken()
	{
		oauthRevokeTokenService.revokeAccessToken(REFRESH_TOKEN_KEY);
		assertThat(oauthTokenStore.readRefreshToken(REFRESH_TOKEN_KEY)).isNotNull();
		assertThat(oauthTokenStore.readAccessToken(ACCESS_TOKEN_KEY)).isNotNull();
	}
}