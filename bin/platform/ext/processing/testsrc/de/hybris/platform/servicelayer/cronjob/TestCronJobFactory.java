/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.cronjob;

import de.hybris.platform.cronjob.model.CleanUpCronJobModel;
import de.hybris.platform.cronjob.model.JobModel;

/**
 * This factory is intended to be just a test factory. Not for a production usage.
 */
public class TestCronJobFactory implements CronJobFactory<CleanUpCronJobModel, JobModel>
{
	@Override
	public CleanUpCronJobModel createCronJob(final JobModel jobModel)
	{
		final CleanUpCronJobModel cronJob = new CleanUpCronJobModel();
		cronJob.setCode("testCleanupCronJob");
		cronJob.setJob(jobModel);

		return cronJob;
	}

}
