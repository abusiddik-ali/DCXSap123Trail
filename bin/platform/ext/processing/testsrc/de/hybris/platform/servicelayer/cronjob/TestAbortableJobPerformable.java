/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.cronjob;


import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;

/**
 * An abortable job for testing.
 */
public class TestAbortableJobPerformable extends AbstractJobPerformable
{
	private boolean shouldAbort = true;

	@Override
	public PerformResult perform(final CronJobModel cronJob)
	{
		if (shouldAbort)
		{
			shouldAbort = false; //it aborts only for the first time
			return new PerformResult(CronJobResult.UNKNOWN, CronJobStatus.ABORTED);
		}

		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	@Override
	public boolean isAbortable()
	{
		return true;
	}
}
