/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.ordercloning;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;

import java.util.List;
import java.util.Map;

public class AdjustEntryTestHook implements CloneAbstractOrderHook
{
	@Override
	public void beforeClone(final AbstractOrderModel original, final Class abstractOrderClassResult)
	{

	}

	@Override
	public <T extends AbstractOrderModel> void afterClone(final AbstractOrderModel original, final T clone,
	                                                      final Class abstractOrderClassResult)
	{

	}

	@Override
	public void beforeCloneEntries(final AbstractOrderModel original)
	{

	}

	@Override
	public <T extends AbstractOrderEntryModel> void afterCloneEntries(final AbstractOrderModel original,
	                                                                  final List<T> clonedEntries)
	{

	}

	@Override
	public void adjustEntryNumbers(final Map<AbstractOrderEntryModel, Integer> entryNumberMappings)
	{
		entryNumberMappings.replaceAll((k, v) -> v * 10);
	}
}

