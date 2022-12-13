/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.web.session.persister;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.web.StoredHttpSessionModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.servicelayer.web.session.HybrisDeserializer;
import de.hybris.platform.servicelayer.web.session.PersistedSession;

import java.util.Collections;
import java.util.List;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;


@IntegrationTest
public class AsyncSessionPersisterTest extends ServicelayerBaseTest
{
	private static final String STORED_SESSIONS_QUERY = "SELECT {PK} FROM {StoredHttpSession}";
	private static final String STORED_SESSION_QUERY = "SELECT {PK} FROM {StoredHttpSession} WHERE {sessionId} = ?id";

	private final HybrisDeserializer deserializer = new HybrisDeserializer(this.getClass().getClassLoader());

	@Resource(name = "defaultAsynchronousSessionPersister")
	AsyncSessionPersister asyncSessionPersister;

	@Resource
	private FlexibleSearchService flexibleSearchService;

	@Resource
	ModelService modelService;

	private long saveInterval;


	@Before
	public void setUp()
	{
		saveInterval = Registry.getCurrentTenant().getConfig().getLong("spring.session.save.async.interval", 4000);
		asyncSessionPersister = Mockito.spy(asyncSessionPersister);
		Mockito.doReturn(true).when(asyncSessionPersister).isAsyncSessionPersistenceConfigured();
		Mockito.doReturn(true).when(asyncSessionPersister).isAllowedToStart();
		asyncSessionPersister.start();
	}

	@After
	public void tearDown()
	{
		asyncSessionPersister.stop();
	}

	@Test
	public void asyncSessionPersisting() throws InterruptedException
	{
		asyncSessionPersister.persist(new PersistedSession("1", 1, "hac", ""));

		sleep();

		final List<StoredHttpSessionModel> storedHttpSessions = getStoredHttpSessions();

		assertThat(storedHttpSessions.size()).isEqualTo(1);
		assertThat(storedHttpSessionListContainsId(storedHttpSessions, "1")).isTrue();
	}

	@Test
	public void asyncSessionPersistingFewSessions() throws InterruptedException
	{
		asyncSessionPersister.persist(new PersistedSession("2", 1, "hac", ""));
		asyncSessionPersister.persist(new PersistedSession("2", 1, "hac", ""));
		asyncSessionPersister.persist(new PersistedSession("3", 1, "hac", ""));

		sleep();

		final List<StoredHttpSessionModel> storedHttpSessions2 = getStoredHttpSessions();

		assertThat(storedHttpSessions2.size()).isEqualTo(2);
		assertThat(storedHttpSessionListContainsId(storedHttpSessions2, "2")).isTrue();
		assertThat(storedHttpSessionListContainsId(storedHttpSessions2, "3")).isTrue();
	}

	@Test
	public void asyncSessionPersistingUpdateSession() throws InterruptedException
	{
		final String attributeName = "testAttribute";

		final PersistedSession persistedSessionBefore = new PersistedSession("4", 1, "hac", "");
		persistedSessionBefore.setAttribute(attributeName, "testValue");
		asyncSessionPersister.persist(persistedSessionBefore);

		sleep();

		final List<StoredHttpSessionModel> storedHttpSessions = getStoredHttpSessions();

		assertThat(storedHttpSessions.size()).isEqualTo(1);
		assertThat(storedHttpSessionListContainsId(storedHttpSessions, "4")).isTrue();
		assertThat((String) getDeserializedSession("4").getAttribute(attributeName)).isEqualTo("testValue");

		final PersistedSession persistedSessionAfter = new PersistedSession("4", 1, "hac", "");
		persistedSessionAfter.setAttribute(attributeName, "testValue2");
		asyncSessionPersister.persist(persistedSessionAfter);

		sleep();

		final List<StoredHttpSessionModel> storedHttpSessions2 = getStoredHttpSessions();

		assertThat(storedHttpSessions2.size()).isEqualTo(1);
		assertThat(storedHttpSessionListContainsId(storedHttpSessions2, "4")).isTrue();
		assertThat((String) getDeserializedSession("4").getAttribute(attributeName)).isEqualTo("testValue2");

	}

	private PersistedSession getDeserializedSession(final String id)
	{
		final SearchResult<StoredHttpSessionModel> result = flexibleSearchService.search(STORED_SESSION_QUERY,
				Collections.singletonMap("id", id));
		final StoredHttpSessionModel sessionModel = result.getResult().get(0);
		modelService.refresh(sessionModel);

		return HybrisDeserializer.deserialize((byte[]) sessionModel.getSerializedSession(), deserializer);
	}

	private List<StoredHttpSessionModel> getStoredHttpSessions()
	{
		final SearchResult<StoredHttpSessionModel> storedHttpSessions = flexibleSearchService.search(STORED_SESSIONS_QUERY);
		return storedHttpSessions.getResult();
	}

	private boolean storedHttpSessionListContainsId(final List<StoredHttpSessionModel> storedHttpSessions, final String id)
	{
		return storedHttpSessions.stream().anyMatch(sessionModel -> sessionModel.getSessionId().equals(id));
	}

	private void sleep() throws InterruptedException
	{
		Thread.sleep(saveInterval * 2);
	}
}
