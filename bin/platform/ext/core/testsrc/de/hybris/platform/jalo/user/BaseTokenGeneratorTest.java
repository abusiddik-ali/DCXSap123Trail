/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jalo.user;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.util.Map;

import org.codehaus.plexus.util.StringUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class BaseTokenGeneratorTest extends TokenGeneratorTest
{

	private final PropertyConfigSwitcher tokenExtendedSwitcher = new PropertyConfigSwitcher("login.token.extended");

	private static final String LANG_ISO = "en";


	@Before
	public void setUp(){
		tokenExtendedSwitcher.switchToValue("false");
		employee = createUser();
	}

	@After
	public void clean(){
		tokenExtendedSwitcher.switchBackToDefault();
	}


	@Test
	public void shouldGenerateSimpleToken() throws Exception{
		final String token = generateToken(100,LANG_ISO);

		final Map<String,String> tokenPartMap = TokenGeneratorProvider.getInstance().getTokenGenerator().decodeToken(token,DELIMITER);

		assertThat(tokenPartMap.get(TokenPart.USER.getKey())).isNotNull();
		assertThat(tokenPartMap.get(TokenPart.PASSWORD.getKey())).isNotNull();
		assertThat(tokenPartMap.get(TokenPart.LANGUAGE.getKey())).isNotNull();
		assertThat(tokenPartMap.get(TokenPart.TTLTIMESTAMP.getKey())).isNull();
		assertThat(tokenPartMap.get(TokenPart.SALT.getKey())).isNull();

		assertThat(StringUtils.isNumeric(tokenPartMap.get(TokenPart.USER.getKey()))).isTrue();
		assertThat(StringUtils.isNumeric(tokenPartMap.get(TokenPart.LANGUAGE.getKey()))).isTrue();

	}


}
