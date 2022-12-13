/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.cronjob.jalo;

import de.hybris.platform.cronjob.jalo.CronJob.CronJobResult;

import java.util.Collections;

import org.apache.log4j.Logger;


public class TestJob extends Job implements TriggerableJob
{
	private Performable performable = null;

	public interface Performable
	{
		CronJobResult perform(CronJob cronJob);
	}

	private static final Logger log = Logger.getLogger(TestJob.class);

	@Override
	protected CronJobResult performCronJob(final CronJob cronJob) throws AbortCronJobException
	{
		if (performable != null)
		{
			return performable.perform(cronJob);
		}
		else
		{
			final long count = ((Long) cronJob.getProperty("count")).longValue();
			log.isDebugEnabled();

			final long start = System.currentTimeMillis();
			for (int i = 0; i < count; i++)
			{
				log.isDebugEnabled();
			}
			final long end = (System.currentTimeMillis() - start);

			log.warn("nested");

			cronJob.setProperty("time", Long.valueOf(end));
			return cronJob.getFinishedResult(true);
		}
	}

	public void setPerformable(final Performable performable)
	{
		this.performable = performable;
	}

	@Override
	public CronJob newExecution()
	{
		return CronJobManager.getInstance().createCronJob(Collections.singletonMap(CronJob.JOB, this));
	}

	public static Logger getLog()
	{
		return log;
	}
}
