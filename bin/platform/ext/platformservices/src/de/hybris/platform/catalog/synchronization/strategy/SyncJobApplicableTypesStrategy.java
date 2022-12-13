/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.synchronization.strategy;


import de.hybris.platform.catalog.model.SyncItemJobModel;
import de.hybris.platform.core.model.ItemModel;


/**
 * An interface that defines a strategy that allows to check whether given item is applicable for the synchronization
 * within given context i.e.
 * <code>syncItemJob</code> and <code>catalogVersionToCheck</code>
 */
public interface SyncJobApplicableTypesStrategy
{
	/**
	 * Check whether given item is applicable for a given context i.e. given <code>syncItemJob </code>.
	 *
	 * @return true when the synchronization is possible on a given item within given context - false otherwise
	 */
	boolean checkIfApplicable(final ItemModel theItem, final SyncItemJobModel syncItemJob);


}
