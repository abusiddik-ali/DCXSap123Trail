/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.processing.distributed.simple;

import de.hybris.platform.core.PK;

import java.util.List;

import com.google.common.base.Preconditions;


public class SometimesFailingBatchProcessor extends TestBatchProcessor
{

	@Override
	protected List<PK> asList(final Object ctx)
	{
		Preconditions.checkState(ctx instanceof List, "ctx must be instance of List");
		final List<PK> result = (List<PK>) ctx;

		if (result.size() < 100)
		{
			throw new IllegalStateException("Test exception");
		}

		return result;
	}
}
