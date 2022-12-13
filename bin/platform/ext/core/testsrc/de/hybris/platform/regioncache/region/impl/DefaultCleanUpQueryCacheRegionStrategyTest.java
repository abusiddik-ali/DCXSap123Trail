/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.regioncache.region.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.Registry;
import de.hybris.platform.jalo.flexiblesearch.FlexibleSearch;
import de.hybris.platform.persistence.flexiblesearch.TranslatedQuery;
import de.hybris.platform.regioncache.CacheConfiguration;
import de.hybris.platform.regioncache.generation.GenerationalCounterService;
import de.hybris.platform.regioncache.generation.impl.TypeCodeGenerationalCounterService;
import de.hybris.platform.regioncache.key.CacheKey;
import de.hybris.platform.regioncache.key.impl.GenerationalCacheDelegate;
import de.hybris.platform.regioncache.region.CacheRegion;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.assertj.core.util.Sets;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@IntegrationTest
public class DefaultCleanUpQueryCacheRegionStrategyTest extends ServicelayerBaseTest
{
	@Mock
	private CacheConfiguration cacheConfiguration;

	@Mock
	private CacheRegion cacheRegion;

	private GenerationalCounterService<String> generationalCounterService;
	private GenerationalCacheDelegate generationalCacheDelegate;
	private DefaultCleanUpQueryCacheRegionStrategy defaultCleanUpQueryCacheRegionStrategy;

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);

		generationalCounterService = new TypeCodeGenerationalCounterService();
		generationalCacheDelegate = new GenerationalCacheDelegate();
		generationalCacheDelegate.setCounterService(generationalCounterService);

		when(cacheConfiguration.getRegions()).thenReturn(List.of(cacheRegion));
		when(cacheRegion.getName()).thenReturn("queryCacheRegion");

		defaultCleanUpQueryCacheRegionStrategy = new DefaultCleanUpQueryCacheRegionStrategy(cacheConfiguration,
				generationalCounterService);
		defaultCleanUpQueryCacheRegionStrategy.afterPropertiesSet();
	}

	@Test
	public void shouldRemoveEntriesFromTheCacheWhenTypeGenerationHasChanged()
	{
		setGenerationCounter(2, 2);

		final CacheKey cacheKey1 = cacheKey(1, 2);
		final CacheKey cacheKey2 = cacheKey(2);
		final CacheKey cacheKey3 = cacheKey(2, 1);
		addKeysToCache(cacheKey1, cacheKey2, cacheKey3);

		setGenerationCounter(1, 1);

		defaultCleanUpQueryCacheRegionStrategy.cleanUp();

		final ArgumentCaptor<CacheKey> cacheKeyArgumentCaptor = ArgumentCaptor.forClass(CacheKey.class);
		verify(cacheRegion, atLeastOnce()).remove(cacheKeyArgumentCaptor.capture(), anyBoolean());

		final List<CacheKey> allValues = cacheKeyArgumentCaptor.getAllValues();
		assertThat(allValues).contains(cacheKey1, cacheKey3);
		assertThat(allValues).doesNotContain(cacheKey2);
	}

	@Test
	public void shouldNotRemoveEntriesFromTheCacheWhenTypeGenerationHasNotChanged()
	{
		setGenerationCounter(2, 2);

		final CacheKey cacheKey1 = cacheKey(1, 2);
		final CacheKey cacheKey2 = cacheKey(2);
		final CacheKey cacheKey3 = cacheKey(2, 1);

		addKeysToCache(cacheKey1, cacheKey2, cacheKey3);

		defaultCleanUpQueryCacheRegionStrategy.cleanUp();

		verify(cacheRegion, never()).remove(any(), anyBoolean());
	}

	protected void addKeysToCache(final CacheKey... keys)
	{
		when(cacheRegion.getAllKeys()).thenReturn(List.of(keys));
	}

	protected void setGenerationCounter(final int typeCode, final long desiredGeneration)
	{
		final String type = String.valueOf(typeCode);
		final String tenantID = Registry.getCurrentTenant().getTenantID();

		final long[] generations = generationalCounterService.getGenerations(new String[]{ type }, tenantID);
		assertThat(generations).hasSize(1);
		final long currentValue = generations[0];
		assertThat(currentValue).isLessThanOrEqualTo(desiredGeneration);

		for (long i = 0; i < desiredGeneration - currentValue; i++)
		{
			generationalCounterService.incrementGeneration(type, tenantID);
		}

	}

	private CacheKey cacheKey(final Integer... dependentTypes)
	{
		final TranslatedQuery translatedQuery = new TranslatedQuery("query", null, null, false, Set.of(), null, false, false);
		final FlexibleSearch.FlexibleSearchCacheKey key = new FlexibleSearch.FlexibleSearchCacheKey(translatedQuery, Map.of(),
				null,
				List.of(PK.class), true, 0, 0,
				Sets.newLinkedHashSet(dependentTypes), false, -1, Registry.getCurrentTenant().getTenantID(), null, List.of());

		generationalCacheDelegate.getGenerationalCacheKey(key);

		return key;
	}
}
