/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.web.session;

import static de.hybris.platform.core.Constants.WEB.JALOSESSION;
import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.web.StoredHttpSessionModel;
import de.hybris.platform.jalo.JaloConnection;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.jalo.security.JaloSecurityException;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.servicelayer.web.session.persister.AsyncSessionPersister;
import de.hybris.platform.servicelayer.web.session.persister.AsyncSessionPersisterRunnable;
import de.hybris.platform.servicelayer.web.session.persister.LinearDrainingAdaptiveAlgorithm;
import de.hybris.platform.servicelayer.web.session.persister.SessionPersister;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.util.List;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.session.SessionRepository;


@IntegrationTest
public class DefaultHybrisSpringSessionRepositoryFactoryPersistingIntegrationTest extends ServicelayerBaseTest
{
	private static final PropertyConfigSwitcher SESSION_SESSION_ENABLED = new PropertyConfigSwitcher("spring.session.enabled");
	private static final PropertyConfigSwitcher SESSION_SAVE_TYPE = new PropertyConfigSwitcher("spring.session.hac.save");
	private static final PropertyConfigSwitcher SESSION_FILTERING_STRATEGY = new PropertyConfigSwitcher(
			"spring.session.hac.filtering.strategy");

	private static final String SYNC = "sync";
	private static final String ASYNC = "async";
	private static final String ANONYMOUS_STRATEGY = "anonymousSessionFilteringStrategy";
	private static final String DEFAULT_STRATEGY = "";
	private static final String STORED_SESSIONS_QUERY = "SELECT {PK} FROM {StoredHttpSession}";

	private final HybrisDeserializer deserializer = new HybrisDeserializer(this.getClass().getClassLoader());

	@Resource
	private DefaultHybrisSpringSessionRepositoryFactory defaultSessionRepositoryFactory;

	@Resource
	private FlexibleSearchService flexibleSearchService;

	@Resource
	private ModelService modelService;

	@Resource
	private HybrisSpringSessionFilter defaultHybrisSpringSessionFilter;

	@Resource
	private SessionPersister asynchronousSessionPersister;

	@Resource
	private LinearDrainingAdaptiveAlgorithm linearDrainingAdaptiveAlgorithm;

	@Resource
	private StoredHttpSessionDao storedHttpSessionDao;

	private long saveInterval;

	@Before
	public void setUp()
	{
		SESSION_SESSION_ENABLED.switchToValue("true");
		saveInterval = Registry.getCurrentTenant().getConfig().getLong("spring.session.save.async.interval", 4000);
	}

	@After
	public void tearDown()
	{
		SESSION_SESSION_ENABLED.switchBackToDefault();
		SESSION_SAVE_TYPE.switchBackToDefault();
		SESSION_FILTERING_STRATEGY.switchBackToDefault();

		defaultSessionRepositoryFactory.setAsynchronousSessionPersister(asynchronousSessionPersister);
		defaultHybrisSpringSessionFilter.init();
	}

	@Test
	public void persistAnonymousSessionSynchronouslyUsingAnonymousStartegy() throws InterruptedException, JaloSecurityException
	{
		SESSION_SAVE_TYPE.switchToValue(SYNC);
		SESSION_FILTERING_STRATEGY.switchToValue(ANONYMOUS_STRATEGY);

		createRepository().save(createHacAnonymousSession());

		verifyNoSessionWasPersisted();
	}

	@Test
	public void persistAnonymousSessionAynchronouslyUsingAnonymousStartegy() throws InterruptedException, JaloSecurityException
	{
		SESSION_SAVE_TYPE.switchToValue(ASYNC);
		SESSION_FILTERING_STRATEGY.switchToValue(ANONYMOUS_STRATEGY);

		createRepository().save(createHacAnonymousSession());

		verifyNoSessionWasPersisted();
	}

	@Test
	public void persistAnonymousSessionSynchronouslyUsingDefaultStartegy() throws InterruptedException, JaloSecurityException
	{
		SESSION_SAVE_TYPE.switchToValue(SYNC);
		SESSION_FILTERING_STRATEGY.switchToValue(DEFAULT_STRATEGY);

		createRepository().save(createHacAnonymousSession());

		verifyThatSessionWasPersisted();
	}

	@Test
	public void persistAnonymousSessionAsynchronouslyUsingDefaultStrategy() throws InterruptedException, JaloSecurityException
	{
		SESSION_SAVE_TYPE.switchToValue(ASYNC);
		SESSION_FILTERING_STRATEGY.switchToValue(DEFAULT_STRATEGY);

		prepareAsyncPersister();
		createRepository().save(createHacAnonymousSession());

		verifyThatSessionWasPersisted();
	}

	@Test
	public void persistAnonymousSessionAsynchronouslyUsingDefaultStrategyForDummyExtension()
			throws InterruptedException, JaloSecurityException
	{
		SESSION_SAVE_TYPE.switchToValue(ASYNC);
		SESSION_FILTERING_STRATEGY.switchToValue(DEFAULT_STRATEGY);

		prepareAsyncPersister();
		createRepository().save(createAnonymousSession("dummy"));

		verifyThatSessionWasPersisted();
	}

	@Test
	public void persistNonAnonymousSessionAsynchronouslyUsingAnonymousStrategy() throws InterruptedException
	{
		SESSION_SAVE_TYPE.switchToValue(ASYNC);
		SESSION_FILTERING_STRATEGY.switchToValue(ANONYMOUS_STRATEGY);

		prepareAsyncPersister();
		createRepository().save(createHacSession());

		verifyThatSessionWasPersisted();
	}

	@Test
	public void persistNonAnonymousSessionSynchronouslyUsingAnonymousStartegy() throws InterruptedException
	{
		SESSION_SAVE_TYPE.switchToValue(SYNC);
		SESSION_FILTERING_STRATEGY.switchToValue(ANONYMOUS_STRATEGY);

		createRepository().save(createHacSession());

		verifyThatSessionWasPersisted();
	}

	@Test
	public void persistNonAnonymousSessionAsynchronouslyUsingDefaultStartegy() throws InterruptedException
	{
		SESSION_SAVE_TYPE.switchToValue(ASYNC);
		SESSION_FILTERING_STRATEGY.switchToValue(DEFAULT_STRATEGY);

		prepareAsyncPersister();
		createRepository().save(createHacSession());

		verifyThatSessionWasPersisted();
	}

	@Test
	public void persistNonAnonymousSessionSynchronouslyUsingDefaultStartegy() throws InterruptedException
	{
		SESSION_SAVE_TYPE.switchToValue(SYNC);
		SESSION_FILTERING_STRATEGY.switchToValue(DEFAULT_STRATEGY);

		createRepository().save(createHacSession());

		verifyThatSessionWasPersisted();
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

	private SessionRepository<PersistedSession> createRepository()
	{
		return defaultSessionRepositoryFactory.createRepository(deserializer, "hac", "/");
	}

	private PersistedSession createHacAnonymousSession() throws JaloSecurityException
	{
		final PersistedSession persistedSession = createHacSession();
		return createAndAssingnAnonymousJaloSession(persistedSession);
	}

	private PersistedSession createAnonymousSession(final String extension) throws JaloSecurityException
	{
		final PersistedSession persistedSession = createSession(extension);
		return createAndAssingnAnonymousJaloSession(persistedSession);
	}

	/**
	 *
	 */
	private PersistedSession createAndAssingnAnonymousJaloSession(final PersistedSession persistedSession)
			throws JaloSecurityException
	{
		final JaloSession anonymousJaloSession = JaloConnection.getInstance().createAnonymousCustomerSession();
		persistedSession.setAttribute(JALOSESSION, anonymousJaloSession);
		return persistedSession;
	}

	private PersistedSession createHacSession()
	{
		return createSession("hac");
	}

	private PersistedSession createSession(final String extension)
	{
		return new PersistedSession("1", 1, extension, "");
	}

	private void verifyNoSessionWasPersisted() throws InterruptedException
	{
		sleep();

		final List<StoredHttpSessionModel> storedHttpSessions = getStoredHttpSessions();

		assertThat(storedHttpSessions.size()).isEqualTo(0);
	}

	private void verifyThatSessionWasPersisted() throws InterruptedException
	{
		sleep();

		final List<StoredHttpSessionModel> storedHttpSessions = getStoredHttpSessions();

		assertThat(storedHttpSessions.size()).isEqualTo(1);
		assertThat(storedHttpSessionListContainsId(storedHttpSessions, "1")).isTrue();
	}

	private void prepareAsyncPersister()
	{
		AsyncSessionPersisterRunnable testAsyncSessionPersisterRunnable;
		TestAsyncSessionPersister testAsynchronousSessionPersister;

		testAsyncSessionPersisterRunnable = new AsyncSessionPersisterRunnable();
		testAsynchronousSessionPersister = new TestAsyncSessionPersister();

		testAsyncSessionPersisterRunnable.setTenant(Registry.getCurrentTenant());
		testAsyncSessionPersisterRunnable.setModelService(modelService);
		testAsyncSessionPersisterRunnable.setDrainingAdaptiveAlgorithm(linearDrainingAdaptiveAlgorithm);
		testAsyncSessionPersisterRunnable.setStoredHttpSessionDao(storedHttpSessionDao);

		testAsyncSessionPersisterRunnable.init();
		testAsynchronousSessionPersister.setAsyncSessionPersisterRunnable(testAsyncSessionPersisterRunnable);

		testAsynchronousSessionPersister.init();
		testAsynchronousSessionPersister.start();
		defaultSessionRepositoryFactory.setAsynchronousSessionPersister(testAsynchronousSessionPersister);
		defaultHybrisSpringSessionFilter.init();
	}

	private class TestAsyncSessionPersister extends AsyncSessionPersister
	{


		@Override
		protected void start()
		{
			super.start();
		}
	}
}
