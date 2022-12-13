/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.processing.distributed.simple;

import de.hybris.platform.core.PK;

import java.util.List;


public class CompletelyFailingBatchProcessor extends TestBatchProcessor
{
	@Override
	protected List<PK> asList(final Object ctx)
	{
		throw new IllegalStateException("Test exception");
	}
}
