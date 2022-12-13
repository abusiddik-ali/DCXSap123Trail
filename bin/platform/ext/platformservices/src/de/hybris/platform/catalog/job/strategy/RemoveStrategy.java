/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.job.strategy;

import de.hybris.platform.servicelayer.cronjob.PerformResult;


/**
 * Strategy abstraction for removing a given instance <code>T</code> related data.
 *
 * @since 4.3
 */
public interface RemoveStrategy<T>
{
	PerformResult remove(T type);
}
