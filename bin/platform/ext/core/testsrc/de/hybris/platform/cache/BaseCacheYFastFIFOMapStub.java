/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.cache;

import de.hybris.platform.util.collections.YFastFIFOMap;


public class BaseCacheYFastFIFOMapStub<K> extends YFastFIFOMap<K, AbstractCacheUnit>
{
	private final CacheBaseStub cacheBase;

	public BaseCacheYFastFIFOMapStub(final CacheBaseStub cacheBase, final int max)
	{
		super(max);
		this.cacheBase = cacheBase;
	}

	@Override
	public void processDisplacedEntry(final Object key, final AbstractCacheUnit value)
	{
		cacheBase.removeUnitFromNestedMapOnly(value);

	}

}
