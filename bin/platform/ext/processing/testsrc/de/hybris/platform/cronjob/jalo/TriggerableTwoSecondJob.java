/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.cronjob.jalo;

import de.hybris.platform.cronjob.jalo.CronJob.CronJobResult;


/**
 * A job for testing, that takes two seconds to run
 */
public class TriggerableTwoSecondJob extends Job implements TriggerableJob
{
	private static String STATICCRONJOBCODE = "TriggerableTestCronJob";

	@Override
	protected CronJobResult performCronJob(final CronJob cronJob) throws AbortCronJobException
	{
		try
		{
			Thread.sleep(2000);
		}
		catch (final Exception e1)
		{
			return cronJob.getFinishedResult(false);
		}
		return cronJob.getFinishedResult(true);
	}

	@Override
	public CronJob newExecution()
	{
		return CronJobManager.getInstance().createCronJob(this, STATICCRONJOBCODE, true);
	}

}
