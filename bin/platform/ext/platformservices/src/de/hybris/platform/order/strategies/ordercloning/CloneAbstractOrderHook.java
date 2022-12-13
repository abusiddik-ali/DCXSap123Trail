/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.ordercloning;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;

import java.util.List;
import java.util.Map;

/**
 * Hook for decorating the abstract order clone process as implemented by the {@link CloneAbstractOrderStrategy}. There
 * are 4 dedicated hooks, that can be used:
 * <ul>
 * <li>before clone of whole abstract order {@link #beforeClone(AbstractOrderModel, Class)}</li>
 * <li>before clone of entry list of abstract order {@link #beforeCloneEntries(AbstractOrderModel)}</li>
 * <li>after clone of entry list of abstract order {@link #afterCloneEntries(AbstractOrderModel, List)}</li>
 * <li>after clone of whole abstract order {@link #afterClone(AbstractOrderModel, AbstractOrderModel, Class)}</li>
 * </ul>
 */
public interface CloneAbstractOrderHook
{
	/**
	 * Hook is executed before document is cloned.
	 *
	 * @param original
	 *           source document
	 * @param abstractOrderClassResult
	 *           target class type
	 */
	public void beforeClone(final AbstractOrderModel original, final Class abstractOrderClassResult);

	/**
	 * Hook is executed after document is cloned.
	 *
	 * @param original
	 *           source document
	 * @param clone
	 *           cloned document
	 * @param abstractOrderClassResult
	 *           target class type
	 */
	public <T extends AbstractOrderModel> void afterClone(final AbstractOrderModel original, final T clone,
			final Class abstractOrderClassResult);


	/**
	 * Hook is executed before entries are cloned.
	 *
	 * @param original
	 *           source document
	 */
	public void beforeCloneEntries(final AbstractOrderModel original);

	/**
	 * Hook is executed after entries are cloned.
	 *
	 * @param original
	 *           source document
	 * @param clonedEntries
	 *           cloned document
	 */
	public <T extends AbstractOrderEntryModel> void afterCloneEntries(final AbstractOrderModel original,
			final List<T> clonedEntries);


	/**
	 * Hook is executed just after copy context creation(before clone).
	 * Entry number should be adjusted in the passed map.
	 *
	 * @param entryNumberMappings source entries with default entry numbers value
	 */
	public default void adjustEntryNumbers(Map<AbstractOrderEntryModel, Integer> entryNumberMappings)
	{
		//NO_OP
	}
}
