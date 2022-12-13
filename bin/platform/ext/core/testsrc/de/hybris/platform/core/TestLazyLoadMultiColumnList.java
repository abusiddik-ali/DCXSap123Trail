/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core;

import java.util.List;
import java.util.Set;


class TestLazyLoadMultiColumnList extends LazyLoadMultiColumnList
{

	public TestLazyLoadMultiColumnList(final List<List<Object>> originalRows, final List<Class> signature,
	                                   final Set<PK> prefetchLanguages, final int prefetchSize, final boolean mustWrapObjectsToo)
	{
		super(originalRows, signature, prefetchLanguages, prefetchSize, mustWrapObjectsToo);

	}

	@Override
	protected Object wrapObject(final Object original)
	{
		return original;
	}


	@Override
	protected LazyLoadItemList createItemList(final Set<PK> prefetchLanguages, final List<PK> itemPKs, final int prefetchSize)
	{
		return new LazyLoadItemList(prefetchLanguages, itemPKs, prefetchSize)
		{
			@Override
			protected List loadPage(final List pks)
			{
				return pks;
			}
		};
	}

}
