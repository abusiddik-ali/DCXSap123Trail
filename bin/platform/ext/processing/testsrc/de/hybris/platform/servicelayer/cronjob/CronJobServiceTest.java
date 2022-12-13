/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.cronjob;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertNull;
import static junit.framework.Assert.assertTrue;
import static junit.framework.Assert.fail;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertFalse;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.jalo.CronJob;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.cronjob.model.JobLogModel;
import de.hybris.platform.cronjob.model.MoveMediaCronJobModel;
import de.hybris.platform.cronjob.model.MoveMediaJobModel;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.exceptions.BusinessException;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.exceptions.SystemException;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.impl.UniqueAttributesInterceptor;
import de.hybris.platform.servicelayer.internal.model.ServicelayerJobModel;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.Arrays;
import java.util.Date;
import java.util.UUID;

import javax.annotation.Resource;

import org.apache.commons.lang.CharUtils;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;


/**
 * More integration tests are in {@link CronJobServiceDemoTest}.
 */
@IntegrationTest
public class CronJobServiceTest extends ServicelayerTransactionalBaseTest
{
	@Resource
	private CronJobService cronJobService;

	@Resource
	private ModelService modelService;

	@Test
	public void testCreate()
	{


		final CronJobModel cronJob = modelService.create("CronJob");
		assertNotNull("cronjob is null", cronJob);
		assertNull("job is not null", cronJob.getJob());
	}

	@Test
	public void testInit() throws InterceptorException
	{
		final CronJobModel cronJob = new CronJobModel();
		assertNull("logtofile is not null", cronJob.getLogToFile());
		modelService.initDefaults(cronJob);
		assertTrue("logtofile option is false", cronJob.getLogToFile().booleanValue());
	}


	@Test
	public void testGetLogTextForOneEntry()
	{
		final ServicelayerJobModel job = modelService.create(ServicelayerJobModel.class);
		job.setCode("testJob");
		job.setSpringId("moveMediaJob");


		final MoveMediaCronJobModel cronJob = modelService.create(MoveMediaCronJobModel.class);
		cronJob.setCode("test");
		cronJob.setJob(job);

		modelService.saveAll(job, cronJob);

		final JobLogModel jobLog = modelService.create(JobLogModel.class);
		jobLog.setCronJob(cronJob);
		jobLog.setLevel(de.hybris.platform.cronjob.enums.JobLogLevel.ERROR);
		final StringBuffer buffer = new StringBuffer(1000);
		for (int i = 0; i < 500; i++)
		{
			buffer.append("Sample message").append(CharUtils.LF);
		}
		jobLog.setMessage(buffer.toString());
		modelService.save(jobLog);

		final String oneEntry = cronJobService.getLogsAsText(cronJob, 1);

		Assert.assertEquals(10, oneEntry.split(String.valueOf(CharUtils.LF)).length);
	}


	@Test
	public void testCronJobCodes()
	{
		final MoveMediaJobModel job = new MoveMediaJobModel();
		job.setCode("mmjm_codetest");
		modelService.save(job);

		assertNotNull("job was null", job);
		assertEquals("not the correct job", "mmjm_codetest", job.getCode());

		final MoveMediaCronJobModel cj1 = modelService.create(MoveMediaCronJobModel.class);
		final MoveMediaCronJobModel cj2 = modelService.create(MoveMediaCronJobModel.class);
		assertNotNull("", cj1);
		assertNotNull("", cj2);
		cj1.setJob(job);
		cj2.setJob(job);
		assertEquals("", job, cj1.getJob());
		assertEquals("", job, cj2.getJob());
		modelService.saveAll(Arrays.asList(cj1, cj2));

		assertNotNull("", cj1.getCode());
		assertNotNull("", cj2.getCode());
		assertFalse("", cj1.getCode().equals(cj2.getCode()));
		assertFalse("", modelService.getSource(cj1).equals(modelService.getSource(cj2)));

		final MoveMediaCronJobModel cj3 = new MoveMediaCronJobModel();
		final MoveMediaCronJobModel cj4 = new MoveMediaCronJobModel();
		cj3.setCode("sameCode");
		cj4.setCode("sameCode");
		cj3.setJob(job);
		cj4.setJob(job);
		try
		{
			modelService.saveAll(Arrays.asList(cj3, cj4));
			fail();
		}
		catch (final ModelSavingException e)
		{
			assertTrue(e.getCause() instanceof InterceptorException);
			final InterceptorException ie = (InterceptorException) e.getCause();
			assertTrue(ie.getInterceptor() instanceof UniqueAttributesInterceptor);
			assertTrue("", e.getMessage().contains("{code=sameCode}"));
		}

		final MoveMediaCronJobModel cj5 = modelService.create(MoveMediaCronJobModel.class);
		cj5.setJob(job);
		modelService.save(cj5);

		assertNotNull("", cj5.getCode());
		assertEquals("", cj5.getCode(), ((CronJob) modelService.getSource(cj5)).getCode());
	}

	@Test
	public void testInvalidCustomCronJobFactory()
	{
		final ServicelayerJobModel job = modelService.create(ServicelayerJobModel.class);
		job.setCode("cleanUpJob");
		job.setSpringId("cleanUpJobPerformable");
		job.setSpringIdCronJobFactory("modelService");
		modelService.save(job);

		try
		{
			cronJobService.getCronJobFactory(job);
			fail();
		}
		catch (final SystemException exception)
		{
			assertTrue("The exception cause should be: 'ClassCastException', but was: "
							+ exception.getCause().getClass().getSimpleName(),
					exception.getCause() instanceof ClassCastException);
		}
	}

	@Test
	public void testNonexistentCustomCronJobFactory()
	{
		final ServicelayerJobModel job = modelService.create(ServicelayerJobModel.class);
		job.setCode("cleanUpJob");
		job.setSpringId("cleanUpJobPerformable");
		//non existing bean id
		job.setSpringIdCronJobFactory("AAA");
		modelService.save(job);

		try
		{
			cronJobService.getCronJobFactory(job);
			fail();
		}
		catch (final SystemException exception)
		{
			assertTrue("The exception cause should be: 'BeansException', but was: "
					+ exception.getCause().getClass().getSimpleName(), exception.getCause() instanceof BeansException);
		}
	}

	@Test
	public void testGetNotExistingJob() throws BusinessException
	{
		try
		{
			cronJobService.getJob("test");
			fail("exception expected");
		}
		catch (final UnknownIdentifierException e)
		{
			// OK
		}

		final MoveMediaJobModel job = new MoveMediaJobModel();
		job.setCode("test");
		modelService.save(job);

		cronJobService.getJob("test");
	}


	@Test
	public void testGetNotExistingCronJob() throws BusinessException
	{
		try
		{
			cronJobService.getCronJob("test");
			fail("exception expected");
		}
		catch (final UnknownIdentifierException e)
		{
			// OK
		}

		final MoveMediaCronJobModel cronJob = new MoveMediaCronJobModel();
		cronJob.setCode("test");

		final MoveMediaJobModel job = new MoveMediaJobModel();
		job.setCode("test");
		modelService.save(job);

		cronJob.setJob(job);
		modelService.save(cronJob);

		cronJobService.getCronJob("test");
	}


	@Test
	public void testServicelayerJobWithNonExistingBean()
	{
		final ServicelayerJobModel job = modelService.create(ServicelayerJobModel.class);
		job.setCode("cleanUpJob");
		job.setSpringId("notExistingSpringId");
		//non existing bean id

		try
		{
			modelService.save(job);
		}
		catch (final SystemException exception)
		{
			fail("We should be able to save an incorrect servicelayerjob");
		}
		try
		{
			cronJobService.getPerformable(job);
			fail("Should throw a NoSuchBeanDefinitionException");
		}
		catch (final NoSuchBeanDefinitionException ses)
		{
			//
		}
	}

	@Test
	public void testServicelayerJobWithEmptyBean()
	{
		final ServicelayerJobModel job = modelService.create(ServicelayerJobModel.class);
		job.setCode("cleanUpJob");
		job.setSpringId("");
		//non existing bean id

		try
		{
			modelService.save(job);
		}
		catch (final SystemException exception)
		{
			fail("We should be able to save an incorrect servicelayerjob");
		}
		try
		{
			cronJobService.getPerformable(job);
			fail("Should throw a NoSuchBeanDefinitionException");
		}
		catch (final NoSuchBeanDefinitionException ses)
		{
			//
		}
	}

	@Test
	public void testServicelayerJobWithNullBean()
	{
		final ServicelayerJobModel job = modelService.create(ServicelayerJobModel.class);
		job.setCode("cleanUpJob");
		job.setSpringId(null);
		//non existing bean id

		try
		{
			modelService.save(job);
			fail("We should not be able to save an servicelayerjob without mandatory springid");
		}
		catch (final SystemException exception)
		{
			//
		}
		try
		{
			cronJobService.getPerformable(job);
			fail("Should throw a IllegalArgumentException");
		}
		catch (final IllegalArgumentException ses)
		{
			//
		}
	}

	@Test
	public void shouldResetStartDateForAbortedCronjob()
	{
		final CronJobModel cronjob = createAbortableCronJob();

		cronJobService.performCronJob(cronjob, true);
		final Date startTime = cronjob.getStartTime();

		assertCronJobAborted(cronjob);

		assertThat(cronjob.getStartTime()).isEqualTo(startTime);
		cronJobService.performCronJob(cronjob, true);

		assertCronJobFinished(cronjob);
		modelService.refresh(cronjob);
		assertThat(cronjob.getStartTime()).isNotNull().isAfter(startTime);
	}

	private CronJobModel createAbortableCronJob()
	{
		final ServicelayerJobModel job = modelService.create(ServicelayerJobModel.class);
		job.setCode(UUID.randomUUID().toString());
		job.setSpringId("testAbortableJobPerformable");

		final CronJobModel cronJob = modelService.create(CronJobModel.class);
		cronJob.setCode(UUID.randomUUID().toString());
		cronJob.setJob(job);

		modelService.saveAll(job, cronJob);
		return cronJob;
	}

	private void assertCronJobAborted(final CronJobModel cronjob)
	{
		final CronJobModel item = modelService.get(cronjob.getPk());
		assertThat(item).isNotNull();
		assertThat(item.getStatus()).isEqualTo(CronJobStatus.ABORTED);
	}

	private void assertCronJobFinished(final CronJobModel cronjob)
	{
		final CronJobModel item = modelService.get(cronjob.getPk());
		assertThat(item).isNotNull();
		assertThat(item.getStatus()).isEqualTo(CronJobStatus.FINISHED);
	}

}
