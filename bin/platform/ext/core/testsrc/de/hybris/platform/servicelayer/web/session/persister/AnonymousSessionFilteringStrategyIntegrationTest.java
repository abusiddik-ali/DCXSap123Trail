/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.web.session.persister;

import static de.hybris.platform.core.Constants.WEB.JALOSESSION;
import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.jalo.JaloConnection;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.jalo.security.JaloSecurityException;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.web.session.PersistedSession;

import javax.annotation.Resource;

import org.junit.Test;


@IntegrationTest
public class AnonymousSessionFilteringStrategyIntegrationTest extends ServicelayerBaseTest
{

	@Resource
	AnonymousSessionFilteringStrategy anonymousSessionFilteringStrategy;

	@Test
	public void checkNullSession()
	{
		try
		{
			anonymousSessionFilteringStrategy.shouldPersist(null);
		}
		catch (final NullPointerException e)
		{
			assertThat(e.getMessage()).isEqualTo("session cannot be null.");
		}
	}

	@Test
	public void checkAnonymousSession() throws JaloSecurityException
	{
		assertThat(anonymousSessionFilteringStrategy.shouldPersist(createAnonymousSession())).isFalse();
	}

	@Test
	public void checkNoAnonymousSession()
	{
		assertThat(anonymousSessionFilteringStrategy.shouldPersist(createSession())).isTrue();
	}

	private PersistedSession createAnonymousSession() throws JaloSecurityException
	{
		final PersistedSession persistedSession = createSession();
		final JaloSession anon = JaloConnection.getInstance().createAnonymousCustomerSession();
		persistedSession.setAttribute(JALOSESSION, anon);
		return persistedSession;
	}

	private PersistedSession createSession()
	{
		return new PersistedSession("1", 1, "hac", "");
	}

}
