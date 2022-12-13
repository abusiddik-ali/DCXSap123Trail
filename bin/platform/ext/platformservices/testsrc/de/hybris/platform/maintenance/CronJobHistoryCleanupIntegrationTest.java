/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.maintenance;


import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.media.MediaFolderModel;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.cronjob.model.MoveMediaCronJobModel;
import de.hybris.platform.cronjob.model.MoveMediaJobModel;
import de.hybris.platform.impex.jalo.ImpExException;
import de.hybris.platform.servicelayer.ServicelayerTest;
import de.hybris.platform.servicelayer.cronjob.CronJobHistoryService;
import de.hybris.platform.servicelayer.cronjob.CronJobService;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.List;

import javax.annotation.Resource;

import org.junit.Test;

@IntegrationTest
public class CronJobHistoryCleanupIntegrationTest extends ServicelayerTest
{
	private static final String TEST_1 = "test1";
	private static final String TEST_2 = "test2";
	private static final String RETENTION_CRON_JOB = "cronJobHistoryRetentionCronJob";

	@Resource
	private CronJobService cronJobService;

	@Resource
	private ModelService modelService;

	@Resource
	private CronJobHistoryService cronJobHistoryService;

	@Test
	public void testPerform() throws ImpExException
	{
		prepareData();

		final int count = countCronJobsHistories(TEST_1, TEST_2);
		assertThat(count).isEqualTo(5);

		importCsv("/impex/cleanup-cronjobhistory.impex", "UTF-8");

		cronJobService.performCronJob(cronJobService.getCronJob(RETENTION_CRON_JOB), true);

		final int countAfterCleanUp = countCronJobsHistories(TEST_1, TEST_2);
		assertThat(countAfterCleanUp).isEqualTo(2);
		assertThat(countCronJobsHistories(RETENTION_CRON_JOB)).isEqualTo(1);
	}

	private void prepareData()
	{
		final CronJobModel cronJob1 = createCronJob(TEST_1);
		final CronJobModel cronJob2 = createCronJob(TEST_2);

		cronJobService.performCronJob(cronJob1, true);
		cronJobService.performCronJob(cronJob1, true);
		cronJobService.performCronJob(cronJob1, true);

		cronJobService.performCronJob(cronJob2, true);
		cronJobService.performCronJob(cronJob2, true);
	}

	private int countCronJobsHistories(final String... codes)
	{
		return cronJobHistoryService.getCronJobHistoryBy(List.of(codes)).size();
	}

	private CronJobModel createCronJob(final String name)
	{
		final MoveMediaCronJobModel cronJob = modelService.create(MoveMediaCronJobModel.class);
		cronJob.setCode(name);

		final MediaFolderModel newFolder = modelService.create(MediaFolderModel.class);
		newFolder.setQualifier(name);
		cronJob.setTargetFolder(newFolder);
		modelService.save(newFolder);

		final MoveMediaJobModel job = new MoveMediaJobModel();
		job.setCode(name);
		cronJob.setJob(job);
		modelService.save(job);

		modelService.save(cronJob);
		return cronJob;
	}
}
