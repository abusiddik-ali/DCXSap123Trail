/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.cronjob.jalo;

import de.hybris.platform.cronjob.jalo.CronJob.CronJobResult;

import org.apache.log4j.Logger;


public class SyncExcutionTestJob extends Job
{
	private static final Logger log = Logger.getLogger(SyncExcutionTestJob.class);

	@Override
	protected CronJobResult performCronJob(final CronJob cronJob) throws AbortCronJobException
	{
		final CronJob cjToPerform = (CronJob) cronJob.getProperty("nested");

		cjToPerform.getJob().perform(cjToPerform, true);

		log.warn("outer");

		return cronJob.getFinishedResult(true);
	}
}
