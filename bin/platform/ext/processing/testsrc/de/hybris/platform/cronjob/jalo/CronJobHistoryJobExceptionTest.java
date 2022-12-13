/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.cronjob.jalo;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.jalo.JaloItemNotFoundException;
import de.hybris.platform.jalo.enumeration.EnumerationValue;
import de.hybris.platform.jalo.type.ComposedType;
import de.hybris.platform.jalo.type.JaloAbstractTypeException;
import de.hybris.platform.jalo.type.JaloGenericCreationException;
import de.hybris.platform.jalo.type.TypeManager;
import de.hybris.platform.scripting.engine.exception.ScriptExecutionException;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import java.util.Collections;
import java.util.UUID;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class CronJobHistoryJobExceptionTest extends ServicelayerBaseTest
{
	private ComposedType composedType;

	@Test
	public void checkImpactOfDifferentExceptionsDuringJobOnCronJobHistory()
			throws JaloAbstractTypeException, AbortCronJobException, JaloGenericCreationException
	{
		final Class<? extends Throwable>[] exceptionsToInvestigate = new Class[]{ RuntimeException.class, ScriptExecutionException.class };

		for (final Class<? extends Throwable> e : exceptionsToInvestigate)
		{
			final CronJob exceptionCronJob = performCronJobAndThrowGivenExceptionDuringPerformance(e);
			final Pair<EnumerationValue, EnumerationValue> cronJobHistoryResultStatus = cronJobHistoryResultStatusPair(
					exceptionCronJob);
			assertThat(exceptionCronJob.getResult()).isEqualTo(cronJobHistoryResultStatus.getLeft());
			assertThat(exceptionCronJob.getStatus()).isEqualTo(cronJobHistoryResultStatus.getRight());
		}
	}

	private Pair<EnumerationValue, EnumerationValue> cronJobHistoryResultStatusPair(final CronJob exceptionCronJob)
	{
		assertThat(exceptionCronJob.getCronJobHistoryEntries().size()).isEqualTo(1);
		final CronJobHistory cronJobHistory = exceptionCronJob.getCronJobHistoryEntries().get(0);
		return Pair.of(cronJobHistory.getResult(), cronJobHistory.getStatus());
	}

	private CronJob performCronJobAndThrowGivenExceptionDuringPerformance(final Class<? extends Throwable> exception)
			throws JaloGenericCreationException, JaloAbstractTypeException, AbortCronJobException
	{
		final String code = randomCode();
		final ExceptionJob exceptionJob = (ExceptionJob) composedType.newInstance(Collections.singletonMap(Job.CODE, code));
		final CronJob exceptionCronJob = CronJobManager.getInstance().createCronJob(exceptionJob, code, true);

		final Job exceptionSpyJob = spy(exceptionCronJob.getJob());
		doThrow(exception).when(exceptionSpyJob).performCronJob(exceptionCronJob);
		exceptionSpyJob.perform(exceptionCronJob, true);
		return exceptionCronJob;
	}

	private String randomCode()
	{
		return UUID.randomUUID().toString();
	}

	@Before
	public void setUp() throws Exception
	{
		final String cronJobExceptionCt = "CronJobException";
		final TypeManager manager = TypeManager.getInstance();
		try
		{
			composedType = manager.getComposedType(cronJobExceptionCt);
		}
		catch (final JaloItemNotFoundException e)
		{
			composedType = manager.createComposedType(manager.getComposedType(Job.class), cronJobExceptionCt);
			composedType.setJaloClass(ExceptionJob.class);
		}
	}

	public static class ExceptionJob extends Job
	{
		@Override
		protected CronJob.CronJobResult performCronJob(final CronJob cronJob)
		{
			return null;
		}
	}
}
