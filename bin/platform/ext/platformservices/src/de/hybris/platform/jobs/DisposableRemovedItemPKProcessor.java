/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jobs;

import de.hybris.platform.core.PK;
import de.hybris.platform.cronjob.model.CronJobModel;

import java.util.Iterator;


/**
 * Abstraction for the {@link Iterator} over {@link PK} with additional init/dispose logic.
 */
public interface DisposableRemovedItemPKProcessor<T extends CronJobModel> extends Iterator<PK>
{
	/**
	 * Method for initializing iterator with {@link CronJobModel} specific data.
	 */
	void init(T model);

	/**
	 * Method for disposing {@link CronJobModel} specific init data ( e.g. closing streams).
	 */
	void dispose();
}
