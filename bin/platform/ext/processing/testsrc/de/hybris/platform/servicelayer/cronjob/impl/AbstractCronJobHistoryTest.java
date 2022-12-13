/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.cronjob.impl;

import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobHistoryModel;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.cronjob.model.JobModel;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.annotation.Resource;

import org.apache.commons.lang.RandomStringUtils;


public abstract class AbstractCronJobHistoryTest extends ServicelayerTransactionalBaseTest
{
	@Resource
	private ModelService modelService;

    private final List<CronJobHistoryModel> cronJobHistoryModelList = new ArrayList<>();
    private final Set<String> randomStringSet = new HashSet<>();
    protected List<CronJobModel> cronJobs = new ArrayList<>();

	protected Date createDate(final int year, final int month, final int day, final int hour, final int min, final int sec)
	{
		final LocalDateTime localDateTime = LocalDateTime.of(year, month, day, hour, min, sec);
		final Date out = Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant());
		return out;
	}

	protected CronJobHistoryModel createEntity(final JobModel jobOne, final UserModel userModel, final Date startDate,
	                                           final Date endDate, final CronJobResult theResult, final CronJobStatus theStatus)
	{
		final CronJobModel cronJob = modelService.create(CronJobModel.class);
		cronJob.setCode(String.format("%s_%s", "cronJobOne", getRandomAndUniqueAlphanumericString(5)));
		cronJob.setJob(jobOne);
		cronJobs.add(cronJob);

		return createEntity(cronJob, jobOne, userModel, startDate, endDate, theResult, theStatus);
	}

	protected CronJobHistoryModel createEntity(final CronJobModel cronJob, final JobModel jobOne, final UserModel userModel,
	                                           final Date startDate, final Date endDate, final CronJobResult theResult,
	                                           final CronJobStatus theStatus)
	{
		final CronJobHistoryModel cronJobHistoryModel = modelService.create(CronJobHistoryModel.class);
		cronJobHistoryModel.setCronJobCode(cronJob.getCode());
		cronJobHistoryModel.setJobCode(jobOne.getCode());
		cronJobHistoryModel.setStartTime(startDate);
		cronJobHistoryModel.setEndTime(endDate);
		cronJobHistoryModel.setUserUid(userModel.getUid());
		cronJobHistoryModel.setCronJob(cronJob);

		cronJobHistoryModel.setResult(theResult);
		cronJobHistoryModel.setStatus(theStatus);


		modelService.saveAll();
		return cronJobHistoryModel;
	}

	protected List<CronJobHistoryModel> getCronJobHistoryModelList()
	{
		return cronJobHistoryModelList;
	}

	protected void addCronJobHistoryModel(final CronJobHistoryModel cronJobHistoryModel)
	{

		cronJobHistoryModelList.add(cronJobHistoryModel);

	}

    protected String getRandomAndUniqueAlphanumericString(final int count)
    {
        String random;
        do
        {
            random = RandomStringUtils.randomAlphanumeric(count);
        } while (randomStringSet.contains(random));
        randomStringSet.add(random);
        return random;
    }
}
