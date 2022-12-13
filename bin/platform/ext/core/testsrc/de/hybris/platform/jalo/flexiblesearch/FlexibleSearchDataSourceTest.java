/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.jalo.flexiblesearch;

import static de.hybris.platform.jalo.flexiblesearch.hints.impl.FlexibleSearchHints.categorizedQuery;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyList;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.anyMap;
import static org.mockito.Matchers.anySetOf;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.atMost;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Constants;
import de.hybris.platform.core.PK;
import de.hybris.platform.jalo.SearchResult;
import de.hybris.platform.jalo.SessionContext;
import de.hybris.platform.jalo.flexiblesearch.hints.Hint;
import de.hybris.platform.jalo.flexiblesearch.hints.impl.FlexibleSearchHints.CategorizedQueryHint;
import de.hybris.platform.jalo.flexiblesearch.internal.FlexibleSearchExecutor;
import de.hybris.platform.jalo.flexiblesearch.internal.ReadOnlyConditionsHelper;
import de.hybris.platform.jdbcwrapper.HybrisDataSource;
import de.hybris.platform.persistence.flexiblesearch.TranslatedQuery;
import de.hybris.platform.testframework.BulkPropertyConfigSwitcher;
import de.hybris.platform.tx.Transaction;
import de.hybris.platform.util.Config;
import de.hybris.platform.util.Utilities;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import javax.sql.DataSource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.mockito.verification.VerificationMode;

@IntegrationTest
public class FlexibleSearchDataSourceTest extends AbstractSwitchingDataSourceTest
{
	private final BulkPropertyConfigSwitcher config = new BulkPropertyConfigSwitcher();
	private SessionContext localCtx;
	private FlexibleSearchExecutor flexibleSearchExecutor;
	private TestFlexibleSearch flexibleSearch;

	@Override
	@Before
	public void setUp() throws Exception
	{
		super.setUp();
		config.switchToValue(ReadOnlyConditionsHelper.PARAM_FS_READ_ONLY_DATASOURCE, "f");
		tenant.getCache().clear();
		CategorizedQueryHint.clearCachedConfig();

		localCtx = jaloSession.createLocalSessionContext();

		this.flexibleSearchExecutor = spy(new FlexibleSearchExecutor(tenant));
		flexibleSearch = createTestFlexibleSearch();

	}

	private TestFlexibleSearch createTestFlexibleSearch()
	{
		return new TestFlexibleSearch(flexibleSearchExecutor);
	}

	@Override
	@After
	public void tearDown()
	{
		super.tearDown();

		if (localCtx != null)
		{
			jaloSession.removeLocalSessionContext();
		}

		config.switchAllBack();
		CategorizedQueryHint.clearCachedConfig();
		tenant.getCache().clear();
	}

	@Test
	public void shouldUseActiveDataSource()
	{
		disableCaching();
		final HybrisDataSource dataSource = tenant.getDataSource();

		flexibleSearch.search(createSampleQuery());

		verifyDataSourceWasUsedOnExecute(dataSource);
	}

	@Test
	public void shouldUseActiveDataSourceWhenProcessSearchResult()
	{
		final HybrisDataSource dataSource = tenant.getDataSource();

		flexibleSearch.processSearchRows(createSampleQuery(), o -> {
		});

		verifyDataSourceWasUsedOnProcessSearchRows(dataSource);
	}

	@Test
	public void shouldUseActiveDataSourceInCache()
	{
		enableCaching();
		final HybrisDataSource dataSource = tenant.getDataSource();

		flexibleSearch.search(createSampleQuery());
		flexibleSearch.search(createSampleQuery());

		verifyDataSourceWasUsedOnExecute(dataSource);
	}

	@Test
	public void shouldUseActiveDataSourceWhenSwitchedToDifferentDS()
	{
		final HybrisDataSource defaultDS = tenant.getDataSource();
		disableCaching();

		final HybrisDataSource otherDS = doWithActivatedSlaveDataSource("a", () -> {

			flexibleSearch.search(createSampleQuery());
			flexibleSearch.search(createSampleQuery());
		});

		verifyDataSourceWasUsedOnExecute(otherDS, 2);
		verifyDataSourcesWereNotUsedOnExecute(defaultDS);
	}

	@Test
	public void shouldUseActiveDataSourceWhenSwitchedToDifferentDSWithProcessSearchRows()
	{
		final HybrisDataSource defaultDS = tenant.getDataSource();

		final HybrisDataSource otherDS = doWithActivatedSlaveDataSource("a", () -> {

			flexibleSearch.processSearchRows(createSampleQuery(), o -> {
			});
			flexibleSearch.processSearchRows(createSampleQuery(), o -> {
			});
		});

		verifyDataSourceWasUsedOnProcessSearchRows(otherDS, 2);
		verifyDataSourcesWereNotUsedOnProcessSearchRows(defaultDS);
	}

	@Test
	public void shouldUseActiveDataSourceWhenSwitchedToDifferentDSWithCache()
	{
		final HybrisDataSource defaultDS = tenant.getDataSource();
		enableCaching();

		final HybrisDataSource otherDS = doWithActivatedSlaveDataSource("a", () -> {

			flexibleSearch.search(createSampleQuery());
			flexibleSearch.search(createSampleQuery());
		});
		verifyDataSourceWasUsedOnExecute(otherDS);
		verifyDataSourcesWereNotUsedOnExecute(defaultDS);
	}

	@Test
	public void shouldUseSameCacheUnitIfSelectedDataSourceHasEqualCachingDomain()
	{
		final HybrisDataSource defaultDS = tenant.getDataSource();

		enableCaching();

		final HybrisDataSource dataSourceD = doWithActivatedSlaveDataSource("d", () -> {
			flexibleSearch.search(createSampleQuery());
			flexibleSearch.search(createSampleQuery());
		});

		//this datasource should not be used as result should already be cached with the same cache domain
		final HybrisDataSource dataSourceE = doWithActivatedSlaveDataSource("e", () -> {
			flexibleSearch.search(createSampleQuery());
			flexibleSearch.search(createSampleQuery());
		});

		verifyDataSourceWasUsedOnExecute(dataSourceD);
		verifyDataSourcesWereNotUsedOnExecute(defaultDS, dataSourceE);
		verifyCacheDomainWasUsed("domainA");
	}

	@Test
	public void shouldUseSameCacheUnitIfSelectedDataSourcesHaveNoCachingDomain()
	{
		final HybrisDataSource defaultDS = tenant.getDataSource();

		enableCaching();

		final HybrisDataSource dataSourceA = doWithActivatedSlaveDataSource("a", () -> {
			flexibleSearch.search(createSampleQuery());
			flexibleSearch.search(createSampleQuery());
		});

		//this datasource should not be used as result should already be cached with the same cache domain
		final HybrisDataSource dataSourceB = doWithActivatedSlaveDataSource("b", () -> {
			flexibleSearch.search(createSampleQuery());
			flexibleSearch.search(createSampleQuery());
		});

		//this should also not hit the datasource as the default datasource has no cache domain defined
		flexibleSearch.search(createSampleQuery());

		verifyDataSourceWasUsedOnExecute(dataSourceA);
		verifyDataSourcesWereNotUsedOnExecute(defaultDS, dataSourceB);
	}

	@Test
	public void shouldUseSeparateCacheUnitIfSelectedDataSourceHasDifferentCachingDomain()
	{
		final HybrisDataSource defaultDS = tenant.getDataSource();

		enableCaching();

		final HybrisDataSource dataSourceD = doWithActivatedSlaveDataSource("d", () -> {
			flexibleSearch.search(createSampleQuery());
			flexibleSearch.search(createSampleQuery());
		});

		//this datasource should be used because this datasource has different caching domain
		final HybrisDataSource dataSourceF = doWithActivatedSlaveDataSource("f", () -> {
			flexibleSearch.search(createSampleQuery());
			flexibleSearch.search(createSampleQuery());
		});

		verifyDataSourceWasUsedOnExecute(dataSourceD);
		verifyDataSourceWasUsedOnExecute(dataSourceF);
		verifyDataSourcesWereNotUsedOnExecute(defaultDS);
	}

	@Test
	public void shouldNotDefineTTLWhenUsingCacheDomainWithoutTTL()
	{
		enableCaching();

		doWithActivatedSlaveDataSource("d", () -> {
			flexibleSearch.search(createSampleQuery());
			flexibleSearch.search(createSampleQuery());
		});

		verifyCacheKeyHasNoTTL();
	}

	@Test
	public void shouldUseDefinedTTLWhenUsingCachingDomainWithTTL()
	{
		enableCaching();

		setCacheDomainTTLProperty("domainB", 60);

		doWithActivatedSlaveDataSource("f", () -> {
			flexibleSearch.search(createSampleQuery());
			flexibleSearch.search(createSampleQuery());
		});

		verifyCacheKeyHasTTL(Duration.ofSeconds(60));
	}

	@Test
	public void shouldInvalidateCacheUnitWithGenerationCounterWhenUsingCachingDomainWithoutTTL()
	{
		enableCaching();

		final HybrisDataSource dataSourceD = doWithActivatedSlaveDataSource("d", () -> {
			flexibleSearch.search(createSampleQuery());
			invokeInvalidationOnType();
			flexibleSearch.search(createSampleQuery());
			flexibleSearch.search(createSampleQuery());
			flexibleSearch.search(createSampleQuery());
		});

		verifyDataSourceWasUsedOnExecute(dataSourceD, 2);
	}

	@Test
	public void shouldNotInvalidateCacheUnitWithGenerationCounterWhenUsingCachingDomainWithTTL()
	{
		enableCaching();

		final Duration expectedTTL = Duration.ofSeconds(100);

		final HybrisDataSource dataSourceD = doWithActivatedSlaveDataSource("f", () -> {
			flexibleSearch.search(createSampleQuery());
			invokeInvalidationOnType();
			flexibleSearch.search(createSampleQuery());
		});

		verifyDataSourceWasUsedOnExecute(dataSourceD, 1);
		verifyCacheKeyHasTTL(expectedTTL);
	}

	@Test
	public void shouldInvalidateCacheUnitWithGenerationCounterWhenUsingTTLFromQuery()
	{
		final Duration testDuration = Duration.ofSeconds(5);

		enableCaching();

		setCacheDomainTTLProperty("domainB", "1");
		localCtx.setAttribute(FlexibleSearch.CACHE_TTL, 2);

		final HybrisDataSource dataSource = doWithActivatedSlaveDataSource("f", () -> {
			final Instant testEnd = Instant.now().plusMillis(testDuration.toMillis());
			while (Instant.now().isBefore(testEnd))
			{
				flexibleSearch.search(createSampleQuery());
				invokeInvalidationOnType();
				sleep(Duration.ofMillis(200));
			}
		});

		verifyDataSourceWasUsedOnExecute(dataSource, atLeast(2));
		verifyDataSourceWasUsedOnExecute(dataSource, atMost(3));
	}

	@Test
	public void shouldNotInvalidateCacheUnitUntilTTLExpiresWhenUsingCachingDomainWithTTL()
	{
		final Duration testDuration = Duration.ofSeconds(3);

		enableCaching();

		setCacheDomainTTLProperty("domainB", "1");

		final HybrisDataSource dataSource = doWithActivatedSlaveDataSource("f", () -> {
			final Instant testEnd = Instant.now().plusMillis(testDuration.toMillis());
			while (Instant.now().isBefore(testEnd))
			{
				flexibleSearch.search(createSampleQuery());
				sleep(Duration.ofMillis(200));
			}
		});

		verifyDataSourceWasUsedOnExecute(dataSource, atLeast(2));
		verifyDataSourceWasUsedOnExecute(dataSource, atMost(4));

	}
	@Test
	public void shouldUseTTLProvidedInQueryWhenUsingCacheDomainWithTTL()
	{
		enableCaching();
		localCtx.setAttribute(FlexibleSearch.CACHE_TTL, 50);

		doWithActivatedSlaveDataSource("f", () -> {

			flexibleSearch.search(createSampleQuery());
			flexibleSearch.search(createSampleQuery());
		});

		verifyCacheKeyHasTTL(Duration.ofSeconds(50));
	}


	@Test
	public void shouldDisableTTLModeIfTTLDefinedForCacheDomainIsZero()
	{

		setCacheDomainTTLProperty("domainB", 0);

		final TestFlexibleSearch flexibleSearch = createTestFlexibleSearch();

		doWithActivatedSlaveDataSource("f", () -> flexibleSearch.search(createSampleQuery()));
		verifyCacheKeyHasNoTTL(flexibleSearch);
	}

	@Test
	public void shouldDisableTTLModeIfTTLDefinedForCacheDomainIsNegative()
	{
		setCacheDomainTTLProperty("domainB", -100);

		final TestFlexibleSearch flexibleSearch = createTestFlexibleSearch();

		doWithActivatedSlaveDataSource("f", () -> flexibleSearch.search(createSampleQuery()));
		verifyCacheKeyHasNoTTL(flexibleSearch);
	}

	@Test
	public void shouldDisableTTLModeIfTTLDefinedForCacheDomainIsEmpty()
	{

		setCacheDomainTTLProperty("domainB", "    ");

		final TestFlexibleSearch flexibleSearch = createTestFlexibleSearch();

		doWithActivatedSlaveDataSource("f", () -> flexibleSearch.search(createSampleQuery()));
		verifyCacheKeyHasNoTTL(flexibleSearch);
	}


	@Test
	public void shouldDisableTTLModeIfTTLDefinedForCacheDomainIsInvalid()
	{
		setCacheDomainTTLProperty("domainB", "thisIsNotANumber");

		final TestFlexibleSearch flexibleSearch = createTestFlexibleSearch();

		doWithActivatedSlaveDataSource("f", () -> flexibleSearch.search(createSampleQuery()));
		verifyCacheKeyHasNoTTL(flexibleSearch);
	}

	@Test
	public void shouldReturnProperDataSourceId()
	{
		disableCaching();

		assertThat(Transaction.current().isRunning()).isFalse();
		assertThat(tenant.isAlternativeMasterDataSource()).isFalse();
		assertThat(tenant.isSlaveDataSource()).isFalse();

		final HybrisDataSource configuredReadOnlyDataSource = getConfiguredReadOnlyDataSource();
		assertThat(configuredReadOnlyDataSource).isNotNull().isNotEqualTo(tenant.getDataSource());

		localCtx.setAttribute(ReadOnlyConditionsHelper.CTX_ENABLE_FS_ON_READ_REPLICA, "true");

		final SearchResult search1 = flexibleSearch.search(createSampleQuery());
		final SearchResult search2 = flexibleSearch.search(createSampleQuery());

		assertThat(search1).isEqualTo(search2);
		assertThat(search1.getDataSourceId()).isEqualTo(configuredReadOnlyDataSource.getID());
		verifyDataSourceWasUsedOnExecute(configuredReadOnlyDataSource, 2);
		verifyDataSourcesWereNotUsedOnExecute(tenant.getDataSource());
	}

	@Test
	public void shouldReturnProperDataSourceIdWithCache()
	{
		enableCaching();

		assertThat(Transaction.current().isRunning()).isFalse();
		assertThat(tenant.isAlternativeMasterDataSource()).isFalse();
		assertThat(tenant.isSlaveDataSource()).isFalse();

		final HybrisDataSource configuredReadOnlyDataSource = getConfiguredReadOnlyDataSource();
		assertThat(configuredReadOnlyDataSource).isNotNull().isNotEqualTo(tenant.getDataSource());

		localCtx.setAttribute(ReadOnlyConditionsHelper.CTX_ENABLE_FS_ON_READ_REPLICA, "true");

		final SearchResult search1 = flexibleSearch.search(createSampleQuery());
		final SearchResult search2 = flexibleSearch.search(createSampleQuery());

		assertThat(search1.getDataSourceId()).isEqualTo(configuredReadOnlyDataSource.getID());
		assertThat(search2.getDataSourceId()).isEqualTo(configuredReadOnlyDataSource.getID());
		verifyDataSourceWasUsedOnExecute(configuredReadOnlyDataSource, 1);
		verifyDataSourcesWereNotUsedOnExecute(tenant.getDataSource());
	}

	@Test
	public void shouldReturnProperDataSourceIdWithAlternativeDataSource()
	{
		disableCaching();

		final AtomicReference<SearchResult> search = new AtomicReference<>();
		doWithActivatedAlternativeDataSource("alt1", () -> {
			search.set(flexibleSearch.search(createSampleQuery()));
		});
		assertThat(search.get()).isNotNull().extracting(SearchResult::getDataSourceId).containsOnly("alt1");
	}


	@Test
	public void shouldReturnProperDataSourceIdWithAlternativeDataSourceWithCache()
	{
		enableCaching();

		final AtomicReference<SearchResult> search = new AtomicReference<>();
		doWithActivatedAlternativeDataSource("alt1", () -> {
			search.set(flexibleSearch.search(createSampleQuery()));
		});
		assertThat(search.get()).isNotNull().extracting(SearchResult::getDataSourceId).containsOnly("alt1");
	}

	@Test
	public void shouldReturnProperDataSourceIdWithSlaveDataSource()
	{
		disableCaching();

		final AtomicReference<SearchResult> search = new AtomicReference<>();
		doWithActivatedSlaveDataSource("a", () -> {
			search.set(flexibleSearch.search(createSampleQuery()));
		});
		assertThat(search.get()).isNotNull().extracting(SearchResult::getDataSourceId).containsOnly("a");
	}

	@Test
	public void shouldReturnProperDataSourceIdWithSlaveDataSourceWithCache()
	{
		enableCaching();

		final AtomicReference<SearchResult> search = new AtomicReference<>();
		doWithActivatedSlaveDataSource("a", () -> {
			search.set(flexibleSearch.search(createSampleQuery()));
		});
		assertThat(search.get()).isNotNull().extracting(SearchResult::getDataSourceId).containsOnly("a");
	}

	@Test
	public void shouldUseReadOnlyDataSourceWithSessionAttributeProperlySet()
	{
		disableCaching();

		assertThat(Transaction.current().isRunning()).isFalse();
		assertThat(tenant.isAlternativeMasterDataSource()).isFalse();
		assertThat(tenant.isSlaveDataSource()).isFalse();

		final HybrisDataSource configuredReadOnlyDataSource = getConfiguredReadOnlyDataSource();
		assertThat(configuredReadOnlyDataSource).isNotNull().isNotEqualTo(tenant.getDataSource());

		localCtx.setAttribute(ReadOnlyConditionsHelper.CTX_ENABLE_FS_ON_READ_REPLICA, "true");

		flexibleSearch.search(createSampleQuery());
		flexibleSearch.search(createSampleQuery());

		verifyDataSourceWasUsedOnExecute(configuredReadOnlyDataSource, 2);
		verifyDataSourcesWereNotUsedOnExecute(tenant.getDataSource());
	}

	@Test
	public void shouldUseReadOnlyDataSourceWithSessionAttributeProperlySetWithCache()
	{
		enableCaching();

		assertThat(Transaction.current().isRunning()).isFalse();
		assertThat(tenant.isAlternativeMasterDataSource()).isFalse();
		assertThat(tenant.isSlaveDataSource()).isFalse();

		final HybrisDataSource configuredReadOnlyDataSource = getConfiguredReadOnlyDataSource();
		assertThat(configuredReadOnlyDataSource).isNotNull().isNotEqualTo(tenant.getDataSource());

		localCtx.setAttribute(ReadOnlyConditionsHelper.CTX_ENABLE_FS_ON_READ_REPLICA, "true");

		flexibleSearch.search(createSampleQuery());
		flexibleSearch.search(createSampleQuery());

		verifyDataSourceWasUsedOnExecute(configuredReadOnlyDataSource);
		verifyDataSourcesWereNotUsedOnExecute(tenant.getDataSource());
	}

	@Test
	public void shouldUseReadOnlyDataSourceOnProcessQueryRowsWithSessionAttributeProperlySet()
	{
		assertThat(Transaction.current().isRunning()).isFalse();
		assertThat(tenant.isAlternativeMasterDataSource()).isFalse();
		assertThat(tenant.isSlaveDataSource()).isFalse();

		final HybrisDataSource configuredReadOnlyDataSource = getConfiguredReadOnlyDataSource();
		assertThat(configuredReadOnlyDataSource).isNotNull().isNotEqualTo(tenant.getDataSource());

		localCtx.setAttribute(ReadOnlyConditionsHelper.CTX_ENABLE_FS_ON_READ_REPLICA, "true");

		flexibleSearch.processSearchRows(createSampleQuery(), (o) -> {
		});
		flexibleSearch.processSearchRows(createSampleQuery(), (o) -> {
		});

		verifyDataSourceWasUsedOnProcessSearchRows(configuredReadOnlyDataSource, 2);
		verifyDataSourcesWereNotUsedOnProcessSearchRows(tenant.getDataSource());
	}

	@Test
	public void shouldUseReadOnlyDataSourceWithProperHint()
	{
		enableCaching();

		setFlexibleSearchCategoryUseReadOnlyProperty("cat1", true);

		assertThat(Transaction.current().isRunning()).isFalse();
		assertThat(tenant.isAlternativeMasterDataSource()).isFalse();
		assertThat(tenant.isSlaveDataSource()).isFalse();

		final HybrisDataSource configuredReadOnlyDataSource = getConfiguredReadOnlyDataSource();
		assertThat(configuredReadOnlyDataSource).isNotNull().isNotEqualTo(tenant.getDataSource());


		flexibleSearch.search(createSampleQuery(categorizedQuery("cat1")));
		flexibleSearch.search(createSampleQuery(categorizedQuery("cat1")));

		verifyDataSourceWasUsedOnExecute(configuredReadOnlyDataSource);
		verifyDataSourcesWereNotUsedOnExecute(tenant.getDataSource());
	}

	@Test
	public void shouldNotUseReadOnlyDataSourceWithSessionAttributeProperlySetAndProperHint()
	{
		enableCaching();
		setFlexibleSearchCategoryUseReadOnlyProperty("cat1", false);

		assertThat(Transaction.current().isRunning()).isFalse();
		assertThat(tenant.isAlternativeMasterDataSource()).isFalse();
		assertThat(tenant.isSlaveDataSource()).isFalse();

		final HybrisDataSource configuredReadOnlyDataSource = getConfiguredReadOnlyDataSource();
		assertThat(configuredReadOnlyDataSource).isNotNull().isNotEqualTo(tenant.getDataSource());

		localCtx.setAttribute(ReadOnlyConditionsHelper.CTX_ENABLE_FS_ON_READ_REPLICA, "true");

		flexibleSearch.search(createSampleQuery(categorizedQuery("cat1")));
		flexibleSearch.search(createSampleQuery(categorizedQuery("cat1")));

		verifyDataSourceWasUsedOnExecute(tenant.getDataSource());
		verifyDataSourcesWereNotUsedOnExecute(configuredReadOnlyDataSource);
	}

	@Test
	public void shouldUseTTLConfiguredForCategoryInQueryWhenHints()
	{
		enableCaching();

		final String queryCategory = "cat1";
		setFlexibleSearchCategoryTTLProperty(queryCategory, 80);

		doWithActivatedSlaveDataSource("a", () -> {

			flexibleSearch.search(createSampleQuery(categorizedQuery(queryCategory)));
			flexibleSearch.search(createSampleQuery(categorizedQuery(queryCategory)));
		});

		verifyCacheKeyHasTTL(Duration.ofSeconds(80));
	}

	@Test
	public void shouldDisableTTLModeIfTTLDefinedForCategoryIsZero()
	{
		final String queryCategory = "cat1";
		setFlexibleSearchCategoryTTLProperty(queryCategory, 0);

		final TestFlexibleSearch flexibleSearch = createTestFlexibleSearch();

		doWithActivatedSlaveDataSource("a", () -> flexibleSearch.search(createSampleQuery(categorizedQuery(queryCategory))));
		verifyCacheKeyHasNoTTL(flexibleSearch);
	}

	@Test
	public void shouldDisableTTLModeIfTTLDefinedForCategoryIsNegative()
	{
		final String queryCategory = "cat1";
		setFlexibleSearchCategoryTTLProperty(queryCategory, -1);

		final TestFlexibleSearch flexibleSearch = createTestFlexibleSearch();

		doWithActivatedSlaveDataSource("a", () -> flexibleSearch.search(createSampleQuery(categorizedQuery(queryCategory))));
		verifyCacheKeyHasNoTTL(flexibleSearch);
	}

	@Test
	public void shouldDisableTTLModeIfTTLDefinedForCategoryIsInvalid()
	{
		final String queryCategory = "cat1";
		setFlexibleSearchCategoryTTLProperty(queryCategory, "notValidInt");

		final TestFlexibleSearch flexibleSearch = createTestFlexibleSearch();

		doWithActivatedSlaveDataSource("a", () -> flexibleSearch.search(createSampleQuery(categorizedQuery(queryCategory))));
		verifyCacheKeyHasNoTTL(flexibleSearch);
	}

	@Test
	public void shouldUseTTLConfiguredForSessionBeforeUsingTTLForCategory()
	{
		enableCaching();
		localCtx.setAttribute(FlexibleSearch.CACHE_TTL, 50);

		final String queryCategory = "cat1";
		setFlexibleSearchCategoryTTLProperty(queryCategory, 80);

		doWithActivatedSlaveDataSource("a", () -> {
			flexibleSearch.search(createSampleQuery(categorizedQuery(queryCategory)));
			flexibleSearch.search(createSampleQuery(categorizedQuery(queryCategory)));
		});

		verifyCacheKeyHasTTL(Duration.ofSeconds(50));
	}

	@Test
	public void shouldUseTTLConfiguredForCategoryBeforeUsingTTLForCacheDomain()
	{
		enableCaching();

		final String queryCategory = "cat1";
		setFlexibleSearchCategoryTTLProperty(queryCategory, 80);
		setCacheDomainTTLProperty("d", 30);

		doWithActivatedSlaveDataSource("d", () -> {
			flexibleSearch.search(createSampleQuery(categorizedQuery(queryCategory)));
			flexibleSearch.search(createSampleQuery(categorizedQuery(queryCategory)));
		});

		verifyCacheKeyHasTTL(Duration.ofSeconds(80));
	}

	@Test
	public void shouldUseLowestTTLConfiguredForCategories()
	{
		enableCaching();

		final String cat1 = "cat1";
		final String cat2 = "cat2";
		final String cat3 = "cat3";
		final String cat4 = "cat4";
		setFlexibleSearchCategoryTTLProperty(cat1, 80);
		setFlexibleSearchCategoryTTLProperty(cat2, 70);
		setFlexibleSearchCategoryTTLProperty(cat3, 90);
		setFlexibleSearchCategoryTTLProperty(cat4, "invalidIntValue");

		doWithActivatedSlaveDataSource("d", () -> {
			flexibleSearch.search(createSampleQuery(categorizedQuery(cat1), categorizedQuery(cat2), categorizedQuery(cat3),
					categorizedQuery(cat4)));
			flexibleSearch.search(createSampleQuery(categorizedQuery(cat1), categorizedQuery(cat2), categorizedQuery(cat3),
					categorizedQuery(cat4)));
		});

		verifyCacheKeyHasTTL(Duration.ofSeconds(70));
	}

	private void setFlexibleSearchCategoryUseReadOnlyProperty(final String category, final boolean b)
	{
		config.switchToValue(
				CategorizedQueryHint.PARAM_FS_CATEGORY_PREFIX + category + CategorizedQueryHint.PARAM_FS_CATEGORY_PREFER_RO_DS_SUFFIX,
				String.valueOf(b));
	}

	private void setFlexibleSearchCategoryTTLProperty(final String category, final int value)
	{
		setFlexibleSearchCategoryTTLProperty(category, Integer.toString(value));
	}

	private void setFlexibleSearchCategoryTTLProperty(final String category, final String value)
	{
		config.switchToValue(
				CategorizedQueryHint.PARAM_FS_CATEGORY_PREFIX + category + CategorizedQueryHint.PARAM_FS_CATEGORY_TTL_SUFFIX,
				value);
	}

	private void setCacheDomainTTLProperty(final String cacheDomainId, final int value)
	{
		setCacheDomainTTLProperty(cacheDomainId, Integer.toString(value));
	}

	private void setCacheDomainTTLProperty(final String cacheDomainId, final String value)
	{
		config.switchToValue(
				FlexibleSearch.PARAM_FLEXIBLE_SEARCH_CACHE_DOMAIN_TTL_PREFIX + cacheDomainId + FlexibleSearch.PARAM_FLEXIBLE_SEARCH_CACHE_DOMAIN_TTL_SUFFIX,
				value);
	}

	private void verifyCacheKeyHasTTL(final Duration ttl)
	{
		final List<FlexibleSearch.FlexibleSearchCacheKey> allValues = flexibleSearch.cacheKeysArgumentCaptor;
		assertThat(allValues).isNotEmpty().allMatch(flexibleSearchCacheKey -> flexibleSearchCacheKey.hasModifier(1 << 2));
		assertThat(allValues).extracting("timeCreated", "timeValidTo")
		                     .extracting(tuple -> Duration.ofMillis(
				                     ((long) tuple.toArray()[1]) - ((long) tuple.toArray()[0])))
		                     .containsOnly(ttl);
	}

	private void verifyCacheKeyHasNoTTL()
	{
		verifyCacheKeyHasNoTTL(flexibleSearch);
	}

	private void verifyCacheKeyHasNoTTL(final TestFlexibleSearch flexibleSearch)
	{
		final List<FlexibleSearch.FlexibleSearchCacheKey> allValues = flexibleSearch.cacheKeysArgumentCaptor;
		assertThat(allValues).isNotEmpty().allMatch(flexibleSearchCacheKey -> !flexibleSearchCacheKey.hasModifier(1 << 2));

		assertThat(allValues).isNotEmpty().extracting("timeCreated", "timeValidTo")
		                     .extracting(tuple -> Duration.ofMillis(
				                     ((long) tuple.toArray()[1]) - ((long) tuple.toArray()[0])))
		                     .allMatch(Duration::isNegative);
	}

	private void verifyCacheDomainWasUsed(final String domainA)
	{
		assertThat(flexibleSearch.cacheKeysArgumentCaptor).extracting("dataSourceCacheDomain").containsOnly(domainA);
	}

	private QueryOptions createSampleQuery(final Hint... hints)
	{
		final QueryOptions.Builder q = QueryOptions.newBuilder().withQuery("select {PK} from {Product}");
		if (hints != null)
		{
			q.withHints(Arrays.asList(hints));
		}
		return q.build();
	}

	private void verifyDataSourcesWereNotUsedOnExecute(final HybrisDataSource... notExpectedDS)
	{

		final ArgumentCaptor<DataSource> dataSourceArgumentCaptor = ArgumentCaptor.forClass(DataSource.class);

		verify(flexibleSearchExecutor, atLeastOnce())
				.execute(anyInt(), anyInt(), anyBoolean(), any(TranslatedQuery.class), anyList(), anyMap(),
						any(PK.class), anyInt(), anySetOf(PK.class), anyListOf(Hint.class), dataSourceArgumentCaptor.capture());

		assertThat(dataSourceArgumentCaptor.getAllValues()).doesNotContain(notExpectedDS);
	}


	private void verifyDataSourcesWereNotUsedOnProcessSearchRows(final HybrisDataSource... notExpectedDS)
	{
		final ArgumentCaptor<DataSource> dataSourceArgumentCaptor = ArgumentCaptor.forClass(DataSource.class);

		verify(flexibleSearchExecutor, atLeastOnce())
				.processSearchRows(anyInt(), anyInt(), any(TranslatedQuery.class), anyList(), anyMap(),
						any(PK.class), anyListOf(Hint.class), any(Consumer.class), dataSourceArgumentCaptor.capture());

		assertThat(dataSourceArgumentCaptor.getAllValues()).doesNotContain(notExpectedDS);
	}

	private void verifyDataSourceWasUsedOnExecute(final HybrisDataSource expectedDS)
	{
		verifyDataSourceWasUsedOnExecute(expectedDS, 1);
	}

	private void verifyDataSourceWasUsedOnExecute(final HybrisDataSource expectedDS,
	                                              final int times)
	{
		verifyDataSourceWasUsedOnExecute(expectedDS, times(times));
	}

	private void verifyDataSourceWasUsedOnExecute(
			final HybrisDataSource expectedDS, final VerificationMode verificationMode)
	{
		verify(flexibleSearchExecutor, verificationMode)
				.execute(anyInt(), anyInt(), anyBoolean(), any(TranslatedQuery.class), anyList(), anyMap(),
						any(PK.class), anyInt(), anySetOf(PK.class), anyListOf(Hint.class), eq(expectedDS));
	}


	private void verifyDataSourceWasUsedOnProcessSearchRows(final HybrisDataSource expectedDS)
	{
		verifyDataSourceWasUsedOnProcessSearchRows(expectedDS, 1);
	}

	private void verifyDataSourceWasUsedOnProcessSearchRows(final HybrisDataSource expectedDS, final int times)
	{
		verify(flexibleSearchExecutor, times(times))
				.processSearchRows(anyInt(), anyInt(), any(TranslatedQuery.class), anyList(), anyMap(),
						any(PK.class), anyListOf(Hint.class), any(Consumer.class), eq(expectedDS));
	}

	private void disableCaching()
	{
		localCtx.setAttribute(FlexibleSearch.DISABLE_CACHE, Boolean.TRUE);
	}

	private void enableCaching()
	{
		localCtx.setAttribute(FlexibleSearch.DISABLE_CACHE, Boolean.FALSE);
	}

	private void invokeInvalidationOnType()
	{
		Utilities.invalidateCache(PK.createCounterPK(Constants.TC.Product));
	}

	private HybrisDataSource getConfiguredReadOnlyDataSource()
	{
		final String configureReadOnlyDataSourceId = Config.getString(ReadOnlyConditionsHelper.PARAM_FS_READ_ONLY_DATASOURCE, "")
		                                                   .toLowerCase(Locale.ROOT);
		assertThat(configureReadOnlyDataSourceId).isNotEmpty();

		final Optional<HybrisDataSource> readOnlyDataSource = tenant.getAllSlaveDataSources()
		                                                            .stream()
		                                                            .filter(ds -> configureReadOnlyDataSourceId.equals(
				                                                            ds.getID()))
		                                                            .findFirst();

		assertThat(readOnlyDataSource).isNotEmpty();
		return readOnlyDataSource.get();
	}

	private void sleep(final Duration sleep)
	{
		try
		{
			Thread.sleep(sleep.toMillis());
		}
		catch (final InterruptedException e)
		{
			throw new RuntimeException(e);
		}
	}

	public static class TestFlexibleSearch extends FlexibleSearch
	{
		List<FlexibleSearchCacheKey> cacheKeysArgumentCaptor = new ArrayList<>();

		public TestFlexibleSearch(final FlexibleSearchExecutor executor)
		{
			super(executor);
		}

		@Override
		public FlexibleSearchCacheUnit createCacheUnit(final int prefetchSize, final Set<PK> languages, final Map _values,
		                                               final FlexibleSearchCacheKey cacheKey)
		{
			cacheKeysArgumentCaptor.add(cacheKey);
			return super.createCacheUnit(prefetchSize, languages, _values, cacheKey);
		}

	}
}
