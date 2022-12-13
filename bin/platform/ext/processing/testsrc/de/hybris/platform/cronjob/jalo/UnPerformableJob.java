/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.cronjob.jalo;

import de.hybris.platform.cronjob.jalo.CronJob.CronJobResult;


/**
 * this job isn't performable, at all!!!
 */
public class UnPerformableJob extends TestJob
{
	@Override
	protected boolean canPerform(final CronJob cronJob)
	{
		return false;
	}

	@Override
	protected CronJobResult performCronJob(final CronJob cronJob)
	{
		throw new IllegalStateException("... calling 'performCronJob(final CronJob cronJob)' shouldn't be possible at all!!!");
	}
}
