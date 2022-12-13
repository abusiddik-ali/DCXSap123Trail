/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.task.impl;

import static java.util.concurrent.TimeUnit.MINUTES;
import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.cronjob.model.TriggerModel;
import de.hybris.platform.scripting.model.ScriptModel;
import de.hybris.platform.scripting.util.TestScriptLogicHolder;
import de.hybris.platform.servicelayer.internal.model.ScriptingJobModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.task.action.TriggerTaskRunner;
import de.hybris.platform.task.impl.gateways.SchedulerStateGateway;
import de.hybris.platform.task.impl.gateways.TasksQueueGateway;
import de.hybris.platform.task.impl.gateways.WorkerStateGateway;

import java.sql.Date;
import java.time.Duration;
import java.time.Instant;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@IntegrationTest
public class TriggerTaskRunnerIntegrationTest extends DefaultTaskServiceBaseTest
{

	private static final Logger LOGGER = LoggerFactory.getLogger(TriggerTaskRunnerIntegrationTest.class);
	@Resource
	private TriggerTaskExecutionStrategy triggerTaskExecutionStrategy;
	@Resource
	private TriggerTaskRunner triggerTaskRunner;
	@Resource
	private AuxiliaryTablesGatewayFactory auxiliaryTablesGatewayFactory;

	private TasksProvider tasksProvider;
	private TestScriptLogicHolder testScripts;

	@Override
	@Before
	public void setUp()
	{
		super.setUp();
		testScripts = new TestScriptLogicHolder();
	}

	@Override
	@After
	public void tearDown()
	{
		testScripts.clear();
		super.tearDown();
	}

	@Test
	public void shouldRetryTriggerTaskWhenProcessedBeforeTriggerActivationTime() throws TimeoutException, InterruptedException
	{
		assureTaskEngineStopped();
		tasksProvider = failOnDeleteTaskProvider();

		final AtomicInteger scriptInvocationCounter = new AtomicInteger();

		final TriggerModel triggerWithScriptJob = createTriggerWithScriptJob(() -> {
			LOGGER.info("{}: executing the script", Instant.now());
			scriptInvocationCounter.incrementAndGet();
		});

		final PK triggerTaskPK = getTriggerTaskPK(triggerWithScriptJob);

		final CountDownLatch processLatch = new CountDownLatch(2);
		final CountDownLatch deleteLatch = new CountDownLatch(0);

		final TestTaskService testTaskService = createTestTaskServiceForTriggerTasks(processLatch, deleteLatch);
		try
		{
			testTaskService.getEngine().start();
			//wait for 2 attempts of processing a trigger (although it is set to be executed once a day)
			//this is possible only if AuxTaskProvider fails on deleting the task from tasks_aux_queue
			assertThat(processLatch.await(2, MINUTES)).isTrue();
		}
		finally
		{
			testTaskService.getEngine().stop();
			modelService.remove(triggerTaskPK);
		}

		//make sure the script was invoked only once (regardless 2 attempts)
		assertThat(scriptInvocationCounter.get()).isEqualTo(1);
	}

	protected TestTaskService createTestTaskServiceForTriggerTasks(final CountDownLatch processLatch,
	                                                               final CountDownLatch deleteLatch)
	{
		final TestExecutionStrategy testExecutionStrategy = new TestExecutionStrategy(taskModel -> true, processLatch,
				deleteLatch,
				triggerTaskExecutionStrategy);
		return new TestTaskService(0, Set.of(), false, true, testExecutionStrategy,
				triggerTaskRunner);
	}

	protected PK getTriggerTaskPK(final TriggerModel triggerWithScriptJob)
	{
		final FlexibleSearchQuery fsq = new FlexibleSearchQuery("GET {TriggerTask} WHERE {trigger}=?trigger");
		fsq.addQueryParameter("trigger", triggerWithScriptJob);
		fsq.setResultClassList(List.of(PK.class));

		final SearchResult<PK> searchResult = flexibleSearchService.search(fsq);
		assertThat(searchResult.getCount()).isEqualTo(1);
		return searchResult.getResult().get(0);
	}

	protected TriggerModel createTriggerWithScriptJob(final TestScriptLogicHolder.TestScriptLogic testScriptLogic)
	{
		final ScriptModel scriptModel = testScripts.scriptWithLogic(modelService, testScriptLogic);

		final ScriptingJobModel scriptingJobModel = prepareScriptingJob(scriptModel.getCode());
		modelService.save(scriptingJobModel);

		final TriggerModel trigger = modelService.create(TriggerModel.class);
		trigger.setJob(scriptingJobModel);
		final ZonedDateTime nextTrigger = ZonedDateTime.now().plus(10, ChronoUnit.SECONDS);

		trigger.setActivationTime(Date.from(nextTrigger.minusDays(1).toInstant()));
		final String cron = String.format("%d %d %d ? * * *", nextTrigger.getSecond(),
				nextTrigger.getMinute(), nextTrigger.getHour());
		trigger.setCronExpression(cron);


		modelService.save(trigger);

		return trigger;
	}

	private TasksProvider failOnDeleteTaskProvider()
	{

		final TestDelegatingAuxiliaryTablesGatewayFactory delegatingAuxiliaryTablesGatewayFactory = new TestDelegatingAuxiliaryTablesGatewayFactory(
				auxiliaryTablesGatewayFactory)
		{
			@Override
			public TasksQueueGateway getTasksQueueGateway()
			{
				return new DelegatingTasksQueueGateway(super.getTasksQueueGateway())
				{
					@Override
					public void deleteTasks(final List<PK> tasks)
					{
						if (tasks == null || tasks.isEmpty())
						{
							return;
						}

						LOGGER.info("do not delete tasks: {}", tasks);
						throw new RuntimeException("not working for testing purposes, tasks to delete: " + tasks);
					}
				};
			}
		};

		final AuxiliaryTablesSchedulerRole schedulerRole = new AuxiliaryTablesSchedulerRole();
		schedulerRole.setGatewayFactory(delegatingAuxiliaryTablesGatewayFactory);
		schedulerRole.setMetricRegistry(metricRegistry);
		schedulerRole.setTypeService(typeService);
		schedulerRole.setWorkerHelper(new DefaultWorkerHelper());
		schedulerRole.afterPropertiesSet();

		final AuxiliaryTablesWorkerRole workerRole = new AuxiliaryTablesWorkerRole();
		workerRole.setGatewayFactory(delegatingAuxiliaryTablesGatewayFactory);
		workerRole.setMetricRegistry(metricRegistry);
		workerRole.afterPropertiesSet();

		final AuxiliaryTablesBasedTaskProvider tasksProvider = new AuxiliaryTablesBasedTaskProvider();
		tasksProvider.setAuxiliaryTablesSchedulerRole(schedulerRole);
		tasksProvider.setAuxiliaryTablesWorkerRole(workerRole);

		return tasksProvider;
	}

	@Override
	protected TasksProvider getTasksProvider()
	{
		return tasksProvider;
	}


	private ScriptingJobModel prepareScriptingJob(final String scriptCode)
	{
		final ScriptingJobModel scriptingJob = modelService.create(ScriptingJobModel.class);
		scriptingJob.setCode("myGroovyJob-" + UUID.randomUUID().toString());
		scriptingJob.setScriptURI("model://" + scriptCode);

		return scriptingJob;

	}

	private static class TestDelegatingAuxiliaryTablesGatewayFactory extends AuxiliaryTablesGatewayFactory
	{

		private final AuxiliaryTablesGatewayFactory delegate;

		private TestDelegatingAuxiliaryTablesGatewayFactory(
				final AuxiliaryTablesGatewayFactory delegate)
		{
			this.delegate = delegate;
		}


		@Override
		public TasksQueueGateway getTasksQueueGateway()
		{
			return delegate.getTasksQueueGateway();
		}

		@Override
		public SchedulerStateGateway getSchedulerStateGateway()
		{
			return delegate.getSchedulerStateGateway();
		}

		@Override
		public WorkerStateGateway getWorkerStateGateway()
		{
			return delegate.getWorkerStateGateway();
		}
	}

	private static class DelegatingTasksQueueGateway implements TasksQueueGateway
	{

		private final TasksQueueGateway delegate;

		private DelegatingTasksQueueGateway(final TasksQueueGateway delegate)
		{
			this.delegate = delegate;
		}

		@Override
		public List<TasksProvider.VersionPK> getTasksForWorkerAndMarkForProcessing(
				final WorkerStateGateway.WorkerRange range, final long maxItemsToSchedule,
				final WorkerStateGateway.WorkerState workerState, final Duration lockDuration)
		{
			return delegate.getTasksForWorkerAndMarkForProcessing(range, maxItemsToSchedule, workerState, lockDuration);
		}

		@Override
		public void clean(final Duration taskProcessingTimeThreshold)
		{
			delegate.clean(taskProcessingTimeThreshold);
		}

		@Override
		public long addTasks(final String tasksQuery, final String expiredTasksQuery, final Instant now, final int rangeStart,
		                     final int rangeEnd)
		{
			return delegate.addTasks(tasksQuery, expiredTasksQuery, now, rangeStart, rangeEnd);
		}

		@Override
		public String defaultIfNull(final String columnName, final Integer defaultValue)
		{
			return delegate.defaultIfNull(columnName, defaultValue);
		}

		@Override
		public String defaultIfNull(final String columnName, final String defaultValue)
		{
			return delegate.defaultIfNull(columnName, defaultValue);
		}

		@Override
		public String getEmptyGroupValue()
		{
			return delegate.getEmptyGroupValue();
		}

		@Override
		public String getRangeSQLExpression(final int rangeStart, final int rangeEnd)
		{
			return delegate.getRangeSQLExpression(rangeStart, rangeEnd);
		}

		@Override
		public List<TasksCountResult> getTasksCount()
		{
			return delegate.getTasksCount();
		}

		@Override
		public List<TasksProvider.VersionPK> getConditionsToSchedule(final String conditionsQuery, final Instant time)
		{
			return delegate.getConditionsToSchedule(conditionsQuery, time);
		}

		@Override
		public String getUnlockTasksStatement()
		{
			return delegate.getUnlockTasksStatement();
		}

		@Override
		public void deleteTasks(final List<PK> tasks)
		{
			delegate.deleteTasks(tasks);
		}

		@Override
		public void unlockTasksForWorkers(final List<Integer> invalidWorkerIds)
		{
			delegate.unlockTasksForWorkers(invalidWorkerIds);
		}

		@Override
		public String getTableName()
		{
			return delegate.getTableName();
		}

		@Override
		public boolean createTable()
		{
			return delegate.createTable();
		}

		@Override
		public void dropTable()
		{
			delegate.dropTable();
		}

		@Override
		public boolean doesTableExist()
		{
			return delegate.doesTableExist();
		}
	}
}
