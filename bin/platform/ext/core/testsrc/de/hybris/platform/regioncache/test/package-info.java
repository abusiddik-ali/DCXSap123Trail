/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
/**
 * Scenario 1: get from cache [no element in cache, one cache region]
 * - default cache region is chosen
 * - unit may be stored in the registry
 * - value is loaded
 * - value is added to cache
 * - eviction may happen
 * - eviction may remove element from registry
 * - cache statistics are updated [miss, ?eviction]
 * <p>
 * Scenario 2: get from cache [element in cache, one cache region]
 * - default cache region is chosen
 * - value is got from cache
 * - cache statistics are updated [hit]
 * <p>
 * Scenario 3: single invalidation [one cache region]
 * - invalidation invoked on default cache region
 * - invalidation filter is checked
 * - invalidation listener is notified (more invalidations can came)
 * - cache registry is checked for more invalidations (if necessary)
 * - key removed from cache registry (if necessary)
 * - value removed from cache
 * - cache statistics are updated [invalidate]
 * <p>
 * Scenario 4: get from cache [no region specified, no element in cache, multiple cache regions{by type} ]
 * - calculated{type} cache region is choosen
 * - unit may be stored in the registry
 * - value is loaded
 * - value is added to cache
 * - eviction may happen
 * - eviction may remove element from registry
 * - cache statistics are updated [miss, ?eviction]
 * <p>
 * Scenario 5: get from cache [no region specified, multiple cache regions{by catalogVersion} ]
 * - exception is thrown "No cache region specified"
 * <p>
 * Scenario 6: get from cache [cache region specified, no element in cache, multiple cache regions{by catalogVersion} ]
 * - specified cache region is chosen
 * - unit may be stored in the registry
 * - value is loaded
 * - value is added to cache
 * - eviction may happen
 * - eviction may remove element from registry
 * - cache statistics are updated [miss, ?eviction]
 * <p>
 * Scenario 7: get from cache - non catalogVersion aware type [cache region specified, no element in cache, multiple cache regions{by catalogVersion} ]
 * - specified cache region is chosen
 * - unit may be stored in the registry
 * - value is loaded
 * - value is added to cache
 * - eviction may happen
 * - eviction may remove element from registry
 * - cache statistics are updated [miss, ?eviction]
 * <p>
 * Scenario 8: single invalidation [no region specified, multiple cache regions{by catalogVersion}]
 * - All regions are invalidated with supplied key
 * <p>
 * Scenario 9: single invalidation [cache region specified, multiple cache regions{by catalogVersion}]
 * * Possibility to specify two cache regions for "all" types which differs only by name
 * - invalidation invoked on specified cache region
 * - invalidation filter is checked
 * - invalidation listener is notified (more invalidations can came even on different regions)
 * - key removed from cache registry (if necessary)
 * - value removed from cache
 * - cache statistics are updated [invalidate]
 * <p>
 * Scenario 10: get from cache [no region specified, multiple cache regions{by catalogVersion, by type} ]
 * - if cache unit type can be stored in more than one cache exception is thrown "No cache region specified"
 * - cache region is calculated
 * - unit may be stored in the registry
 * - value is loaded
 * - value is added to cache
 * - eviction may happen
 * - eviction may remove element from registry
 * - cache statistics are updated [miss, ?eviction]
 * <p>
 * Scenario 11: single invalidation [no region specified, multiple cache regions{by catalogVersion, by type} ]
 * - if cache unit type can be stored in more than one cache exception is thrown "No cache region specified"
 * - cache region is calculated
 * - invalidation invoked on specified cache region
 * - invalidation filter is checked
 * - invalidation listener is notified (more invalidations can came even on different regions)
 * - key removed from cache registry (if necessary)
 * - value removed from cache
 * - cache statistics are updated [invalidate]
 * <p>
 * Scenario 12:
 * - Flexible search queries for online catalog are kept in region[1]
 * - Flexible search queries for staging catalog are kept in region[2]
 * - Invalidation coming for region[3]
 * - Expected: Invalidation is performed for relevant keys in region[1] AND region[2]
 * <p>
 * * not covered:
 * - impex
 * - ...
 */
package de.hybris.platform.regioncache.test;

