/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test;

import de.hybris.platform.core.LazyLoadItemList;
import de.hybris.platform.core.PK;

import java.util.List;
import java.util.Set;


/**
 * Test class with monitor for not empty page buffer.
 */
public class MonitoredLazyLoadItemList<E> extends LazyLoadItemList<E>
{
	private transient boolean notemptyPageBuffer = false;

	public MonitoredLazyLoadItemList(final Set<PK> prefetchLanguages, final List<PK> pks, final int prefetchSize)
	{
		super(prefetchLanguages, pks, prefetchSize);
	}

	@Override
	protected E getBuffered(final int listPos)
	{
		final E result = super.getBuffered(listPos);
		final BufferedPage<E> page = getBufferedPageIfLoaded(listPos);
		notemptyPageBuffer = page != null && !page.isEmpty();
		return result;
	}

	public boolean isNotEmptyPageBuffer()
	{
		return notemptyPageBuffer;
	}

}
