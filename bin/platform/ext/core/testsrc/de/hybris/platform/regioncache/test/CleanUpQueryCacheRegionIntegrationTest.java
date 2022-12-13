/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.regioncache.test;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.jalo.flexiblesearch.FlexibleSearch;
import de.hybris.platform.regioncache.CacheConfiguration;
import de.hybris.platform.regioncache.key.CacheKey;
import de.hybris.platform.regioncache.key.legacy.AbstractLegacyRegistrableCacheKey;
import de.hybris.platform.regioncache.region.impl.DefaultCleanUpQueryCacheRegionService;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.servicelayer.type.TypeService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.util.Collection;
import java.util.Collections;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class CleanUpQueryCacheRegionIntegrationTest extends ServicelayerBaseTest
{
	@Resource
	private DefaultCleanUpQueryCacheRegionService defaultCleanUpQueryCacheRegionService;

	@Resource
	private ModelService modelService;

	@Resource
	private TypeService typeService;

	@Resource
	private FlexibleSearchService flexibleSearchService;

	@Resource
	private CacheConfiguration cacheConfiguration;

	private final static String CODE = "queryRegionTest";
	private final PropertyConfigSwitcher switcher = new PropertyConfigSwitcher("queryRegionCache.clean.enabled");

	@Before
	public void before()
	{
		switcher.switchToValue("false");
	}

	@After
	public void after()
	{
		switcher.switchBackToDefault();
	}

	@Test
	public void testFlexibleSearchCacheCleaning()
	{
		// when
		selectAllTitles(0);
		createNewTitle();

		// then
		selectAllTitles(1);
		assertCache(2);

		// when
		createNewTitle();

		// then
		selectAllTitles(2);
		assertCache(3);

		// when
		createNewTitle();

		// then
		selectAllTitles(3);
		assertCache(4);

		// when
		defaultCleanUpQueryCacheRegionService.clearCache();

		// then
		assertCache(1);
	}

	@Test
	public void testLegacyCacheCleaning()
	{
		// given
		long numberOfStatements = countItemsInLegacyCache();

		// when
		createNewComposedType();

		// then
		assertThat(numberOfStatements).isLessThanOrEqualTo(countItemsInLegacyCache());

		// when
		numberOfStatements = countItemsInLegacyCache();
		defaultCleanUpQueryCacheRegionService.clearCache();

		// then
		assertThat(numberOfStatements).isGreaterThan(countItemsInLegacyCache());
	}

	private void assertCache(final int expectedCount)
	{
		final AtomicInteger counter = new AtomicInteger();
		final Collection<CacheKey> allKeys = cacheConfiguration.getRegions()
		                                                       .stream()
		                                                       .filter(region -> region.getName().equals("queryCacheRegion"))
		                                                       .findFirst()
		                                                       .get()
		                                                       .getAllKeys();

		final long count = allKeys.stream()
		                          .filter(key -> key instanceof FlexibleSearch.FlexibleSearchCacheKey &&
				                          key.toString().contains(CODE) &&
				                          ((FlexibleSearch.FlexibleSearchCacheKey) key).getDependentTypes()[0].equals("24"))
		                          .count();

		assertThat(count).isEqualTo(expectedCount);
	}

	private long countItemsInLegacyCache()
	{
		final Collection<CacheKey> allKeys = cacheConfiguration.getRegions()
		                                                       .stream()
		                                                       .filter(region -> region.getName().equals("queryCacheRegion"))
		                                                       .findFirst()
		                                                       .get()
		                                                       .getAllKeys();

		return allKeys.stream().filter(key -> key instanceof AbstractLegacyRegistrableCacheKey).count();
	}

	private void createNewTitle()
	{
		final TitleModel title = modelService.create(TitleModel.class);
		title.setCode(CODE + UUID.randomUUID().toString());
		modelService.save(title);
	}

	private void createNewComposedType()
	{
		final ComposedTypeModel title = (ComposedTypeModel) typeService.getTypeForCode("Title");
		final ComposedTypeModel newType = modelService.create(ComposedTypeModel.class);
		newType.setCode("New" + UUID.randomUUID().toString().substring(0, 5));
		newType.setSuperType(title);
		newType.setSingleton(false);
		newType.setGenerate(false);
		newType.setCatalogItemType(false);
		modelService.save(newType);
	}


	private void selectAllTitles(final int expectedCount)
	{
		final FlexibleSearchQuery query = new FlexibleSearchQuery("Select {pk} from {Title} where {code} like '" + CODE + "%'");
		query.setResultClassList(Collections.singletonList(TitleModel.class));
		final SearchResult<Object> searchResult = flexibleSearchService.search(query);
		assertThat(searchResult.getCount()).isEqualTo(expectedCount);
	}
}