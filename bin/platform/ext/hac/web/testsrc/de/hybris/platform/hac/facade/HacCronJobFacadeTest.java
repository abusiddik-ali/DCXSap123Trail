/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.hac.facade;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.cronjob.model.CronJobHistoryModel;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.cronjob.model.JobModel;
import de.hybris.platform.hac.data.dto.CronJobData;
import de.hybris.platform.servicelayer.cronjob.CronJobHistoryService;
import de.hybris.platform.servicelayer.cronjob.CronJobService;
import de.hybris.platform.servicelayer.exceptions.SystemException;

import java.time.Instant;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;


@UnitTest
public class HacCronJobFacadeTest
{
	private static final String JOB_CODE2 = "JobCode2";
	private static final String JOB_CODE1 = "JobCode1";
	private static final String CRON_JOB_CODE2 = "CronJobCode2";
	private static final String CRON_JOB_CODE1 = "CronJobCode1";
	private static final int EXPIRATION_TIME = 5;

	@InjectMocks
	private final HacCronJobFacade cronJobFacade = new HacCronJobFacade(
			EXPIRATION_TIME);
	@Mock
	private CronJobService cronJobService;
	@Mock
	private CronJobHistoryService cronJobHistoryService;
	@Mock
	private CronJobModel cronJob1, cronJob2;
	@Mock
	private JobModel job1, job2;
	@Mock
	private CronJobHistoryModel cronJobHistory1, cronJobHistory2;

	@Before
	public void setUp()
	{
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void shouldReturnEmptyResultListWhenThereIsNoRunningCronJobs()
	{
		// given
		given(cronJobService.getRunningOrRestartedCronJobs()).willReturn(ImmutableList.of());

		// when
		final List<CronJobData> result = cronJobFacade.getRunningOrRestartedCronJobsData();

		// then
		assertThat(result).isEmpty();
		verify(cronJobService).getRunningOrRestartedCronJobs();
	}

	@Test
	public void shouldReturnEmptyResultListWhenCronJobServiceThrewSystemException()
	{
		// given
		given(cronJobService.getRunningOrRestartedCronJobs())
				.willThrow(new SystemException("Returned result for running cron jobs is null "));

		// when
		final List<CronJobData> result = cronJobFacade.getRunningOrRestartedCronJobsData();

		// then
		assertThat(result).isEmpty();
		verify(cronJobService).getRunningOrRestartedCronJobs();
	}

	@Test
	public void shouldNotReturnChangedResultListBeforeExpirationTime() throws InterruptedException
	{
		given(cronJobService.getRunningOrRestartedCronJobs()).willReturn(ImmutableList.of());

		final List<CronJobData> result1 = cronJobFacade.getRunningOrRestartedCronJobsData();
		given(cronJobService.getRunningOrRestartedCronJobs()).willReturn(Lists.newArrayList(cronJob1, cronJob2));
		TimeUnit.SECONDS.sleep(EXPIRATION_TIME - 1);
		final List<CronJobData> result2 = cronJobFacade.getRunningOrRestartedCronJobsData();
		assertThat(result2).isEmpty();
		assertThat(result1).isSameAs(result2);
		verify(cronJobService).getRunningOrRestartedCronJobs();
	}

	@Test
	public void shouldReturnChangedResultListAfterExpirationTime() throws InterruptedException
	{
		given(cronJobService.getRunningOrRestartedCronJobs()).willReturn(ImmutableList.of());
		final List<CronJobData> result1 = cronJobFacade.getRunningOrRestartedCronJobsData();

		given(cronJobService.getRunningOrRestartedCronJobs()).willReturn(Lists.newArrayList(cronJob1, cronJob2));
		mockCronJobs();

		TimeUnit.SECONDS.sleep(EXPIRATION_TIME + 1);

		final List<CronJobData> result2 = cronJobFacade.getRunningOrRestartedCronJobsData();
		assertThat(result1).isEmpty();
		assertThat(result2).hasSize(2);
		assertThat(result1).isNotSameAs(result2);
		verify(cronJobService, times(2)).getRunningOrRestartedCronJobs();
	}

	@Test
	public void shouldReturnEmptyResultListWhenThereArRunningCronJobs()
	{
		// given
		given(cronJobService.getRunningOrRestartedCronJobs()).willReturn(Lists.newArrayList(cronJob1, cronJob2));
		given(cronJob1.getCode()).willReturn(CRON_JOB_CODE1);
		given(cronJob2.getCode()).willReturn(CRON_JOB_CODE2);
		given(cronJob1.getJob()).willReturn(job1);
		given(cronJob2.getJob()).willReturn(job2);
		given(cronJob1.getStartTime()).willReturn(Date.from(Instant.now()));
		given(cronJob2.getStartTime()).willReturn(Date.from(Instant.now()));
		given(cronJob1.getActiveCronJobHistory()).willReturn(cronJobHistory1);
		given(cronJob2.getActiveCronJobHistory()).willReturn(cronJobHistory2);
		given(cronJobHistory1.getProgress()).willReturn(20d);
		given(cronJobHistory2.getProgress()).willReturn(10d);
		given(job1.getCode()).willReturn(JOB_CODE1);
		given(job2.getCode()).willReturn(JOB_CODE2);

		// when
		final List<CronJobData> result = cronJobFacade.getRunningOrRestartedCronJobsData();

		// then
		assertThat(result).hasSize(2);
		verify(cronJobService).getRunningOrRestartedCronJobs();
	}

	@Test
	public void shouldReturnEmptyResultMapWhenThereIsNoRunningCronJobsToAbort()
	{
		// given
		given(cronJobService.getRunningOrRestartedCronJobs()).willReturn(ImmutableList.of());

		// when
		final Map<String, Boolean> requestResult = cronJobFacade.requestAbortForRunningCronJobs();

		// then
		assertThat(requestResult).isEmpty();
		verify(cronJobService).getRunningOrRestartedCronJobs();
	}

	@Test
	public void shouldReturnEmptyResultMapWhenCronJobServiceThrewSystemException()
	{
		// given
		given(cronJobService.getRunningOrRestartedCronJobs())
				.willThrow(new SystemException("Returned result for running cron jobs is null "));

		// when
		final Map<String, Boolean> requestResult = cronJobFacade.requestAbortForRunningCronJobs();

		// then
		assertThat(requestResult).isEmpty();
		verify(cronJobService).getRunningOrRestartedCronJobs();
	}

	@Test
	public void shouldReturnEmptyResultMapWhenCronJobServiceHasReturnedRunningCronJobsButNonOfThemHasFlagIsRunning()
	{
		// given
		given(cronJobService.getRunningOrRestartedCronJobs()).willReturn(Lists.newArrayList(cronJob1, cronJob2));
		given(cronJobService.isRunning(cronJob1)).willReturn(Boolean.FALSE);
		given(cronJobService.isRunning(cronJob2)).willReturn(Boolean.FALSE);

		// when
		final Map<String, Boolean> requestResult = cronJobFacade.requestAbortForRunningCronJobs();

		// then
		assertThat(requestResult).isEmpty();
		verify(cronJobService).getRunningOrRestartedCronJobs();
		verify(cronJobService, times(2)).isRunning(any(CronJobModel.class));
	}

	@Test
	public void shouldRequestAbortForTwoRunningCronJobsIfTheyAreRunningAndAreAbortableAndReturnResulMapWithSizeOfTwo()
	{
		// given
		given(cronJobService.getRunningOrRestartedCronJobs()).willReturn(Lists.newArrayList(cronJob1, cronJob2));
		given(cronJobService.isRunning(cronJob1)).willReturn(Boolean.TRUE);
		given(cronJobService.isRunning(cronJob2)).willReturn(Boolean.TRUE);
		given(cronJob1.getCode()).willReturn(CRON_JOB_CODE1);
		given(cronJob2.getCode()).willReturn(CRON_JOB_CODE2);

		// when
		final Map<String, Boolean> requestResult = cronJobFacade.requestAbortForRunningCronJobs();

		// then
		assertThat(requestResult).isNotEmpty();
		assertThat(requestResult).hasSize(2);
		assertThat(requestResult.get(CRON_JOB_CODE1)).isEqualTo(Boolean.TRUE);
		assertThat(requestResult.get(CRON_JOB_CODE2)).isEqualTo(Boolean.TRUE);
		verify(cronJobService).getRunningOrRestartedCronJobs();
		verify(cronJobService, times(2)).isRunning(any(CronJobModel.class));
		verify(cronJobService, times(2)).requestAbortCronJob(any(CronJobModel.class));
	}

	@Test
	public void shouldRequestAbortForTwoRunningCronJobsIfTheyAreRunningAndOneIsAbortableAndAnotherNotAndReturnResulMapWithSizeOfTwo()
	{
		// given
		given(cronJobService.getRunningOrRestartedCronJobs()).willReturn(Lists.newArrayList(cronJob1, cronJob2));
		given(cronJobService.isRunning(cronJob1)).willReturn(Boolean.TRUE);
		given(cronJobService.isRunning(cronJob2)).willReturn(Boolean.TRUE);
		willThrow(new IllegalStateException()).given(cronJobService).requestAbortCronJob(cronJob2);
		given(cronJob1.getCode()).willReturn(CRON_JOB_CODE1);
		given(cronJob2.getCode()).willReturn(CRON_JOB_CODE2);

		// when
		final Map<String, Boolean> requestResult = cronJobFacade.requestAbortForRunningCronJobs();

		// then
		assertThat(requestResult).isNotEmpty();
		assertThat(requestResult).hasSize(2);
		assertThat(requestResult.get(CRON_JOB_CODE1)).isEqualTo(Boolean.TRUE);
		assertThat(requestResult.get(CRON_JOB_CODE2)).isEqualTo(Boolean.FALSE);
		verify(cronJobService).getRunningOrRestartedCronJobs();
		verify(cronJobService, times(2)).isRunning(any(CronJobModel.class));
		verify(cronJobService, times(2)).requestAbortCronJob(any(CronJobModel.class));
	}

	private void mockCronJobs()
	{
		given(cronJob1.getCode()).willReturn(CRON_JOB_CODE1);
		given(cronJob2.getCode()).willReturn(CRON_JOB_CODE2);
		given(cronJob1.getJob()).willReturn(job1);
		given(cronJob2.getJob()).willReturn(job2);
		given(cronJob1.getStartTime()).willReturn(Date.from(Instant.now()));
		given(cronJob2.getStartTime()).willReturn(Date.from(Instant.now()));
		given(cronJob1.getActiveCronJobHistory()).willReturn(cronJobHistory1);
		given(cronJob2.getActiveCronJobHistory()).willReturn(cronJobHistory2);
		given(cronJobHistory1.getProgress()).willReturn(20d);
		given(cronJobHistory2.getProgress()).willReturn(10d);
		given(job1.getCode()).willReturn(JOB_CODE1);
		given(job2.getCode()).willReturn(JOB_CODE2);
	}

}
