/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.core.PK;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.type.TypeService;
import de.hybris.platform.task.RetryLaterException;
import de.hybris.platform.task.TaskModel;
import de.hybris.platform.task.TaskRunner;
import de.hybris.platform.task.TaskService;
import de.hybris.platform.task.TestTaskRunner;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.Config;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;

import javax.annotation.Resource;

import org.jgroups.util.Util;
import org.junit.After;
import org.junit.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.retry.support.RetryTemplate;

import com.codahale.metrics.MetricRegistry;
import com.google.common.collect.Lists;

public abstract class DefaultTaskServiceBaseTest extends ServicelayerBaseTest
{
	private static final int DEFAULT_SHUTDOWN_WAIT = 15;
	private static final String TASK_ENGINE_EXCLUSIVE_MODE = "task.engine.exclusive.mode";
	private static final Logger LOGGER = LoggerFactory.getLogger(DefaultTaskServiceBaseTest.class);
	protected final Set<PK> tasksCreated = new LinkedHashSet<>();
	private final PropertyConfigSwitcher taskEngineExclusiveModeSwitch = new PropertyConfigSwitcher(TASK_ENGINE_EXCLUSIVE_MODE);
	@Resource
	protected ModelService modelService;
	@Resource
	protected FlexibleSearchService flexibleSearchService;
	@Resource
	protected TaskService taskService;
	@Resource
	protected TypeService typeService;
	@Resource
	protected MetricRegistry metricRegistry;
	@Resource
	protected RetryTemplate taskEngineRetryTemplate;
	private boolean taskEngineWasRunningBefore;

	@Before
	public void setUp()
	{
		if (taskService.getEngine().isRunning())
		{
			taskEngineWasRunningBefore = true;
			taskService.getEngine().stop();
		}
	}

	@After
	public void tearDown()
	{
		//before starting task engine, try to remove all created tasks
		tasksCreated.forEach(t -> {
			try
			{
				modelService.remove(t);
				LOGGER.info("task {} removed", t);
			}
			catch (final Exception ignore)
			{
				//ignore
			}
		});


		resetExclusiveMode();
		if (taskEngineWasRunningBefore)
		{
			taskService.getEngine().start();
		}
	}

	protected void markTaskForRemovalAfterTest(final PK pk)
	{
		LOGGER.info("task {} will be removed after test", pk);
		tasksCreated.add(pk);
	}

	private Long createTask(final Function<TaskModel, TaskModel> extend)
	{
		final TaskModel task = extend.apply(modelService.create(TaskModel.class));
		task.setRunnerBean("runner");
		taskService.scheduleTask(task);
		markTaskForRemovalAfterTest(task.getPk());
		return task.getPk().getLong();
	}

	private Long createExpiredTask(final Function<TaskModel, TaskModel> extend)
	{
		return createTask(extend.compose(task -> {
			task.setExpirationDate(Date.from(Instant.now().minus(1, ChronoUnit.DAYS)));
			task.setExecutionDate(Date.from(Instant.now().minus(2, ChronoUnit.DAYS)));
			return task;
		}));
	}

	protected Long createTask(final String group, final UnaryOperator<TaskModel> extend)
	{
		return createTask(extend.compose(task -> {
			task.setNodeGroup(group);
			return task;
		}));
	}

	protected Long createExpiredTask(final String group, final UnaryOperator<TaskModel> extend)
	{
		return createExpiredTask(extend.compose(task -> {
			task.setNodeGroup(group);
			return task;
		}));
	}

	protected Long createTask(final int nodeId, final UnaryOperator<TaskModel> extend)
	{
		return createTask(extend.compose(task -> {
			task.setNodeId(nodeId);
			return task;
		}));
	}

	protected Long createExpiredTask(final int nodeId, final UnaryOperator<TaskModel> extend)
	{
		return createExpiredTask(extend.compose(task -> {
			task.setNodeId(nodeId);
			return task;
		}));
	}

	protected Long createTask(final int nodeId, final String group, final UnaryOperator<TaskModel> extend)
	{
		return createTask(extend.compose(task -> {
			task.setNodeId(nodeId);
			task.setNodeGroup(group);
			return task;
		}));
	}

	protected Long createTask()
	{
		return createTask(t -> t);
	}

	protected Long createTask(final String group)
	{
		return createTask(group, t -> t);
	}

	protected Long createExpiredTask(final String group)
	{
		return createExpiredTask(group, t -> t);
	}

	protected Long createTask(final int nodeId)
	{
		return createTask(nodeId, t -> t);
	}

	protected Long createExpiredTask(final int nodeId)
	{
		return createExpiredTask(nodeId, t -> t);
	}

	protected Long createTask(final int nodeId, final String group)
	{
		return createTask(nodeId, group, t -> t);
	}

	protected abstract TasksProvider getTasksProvider();

	protected void assureTaskEngineStopped() throws TimeoutException
	{
		Util.waitUntil(DEFAULT_SHUTDOWN_WAIT * 1000, 1000, () -> !taskService.getEngine().isRunning());
		assertThat(taskService.getEngine().isRunning())
				.overridingErrorMessage("Task Engine failed to shut down in " + DEFAULT_SHUTDOWN_WAIT + " seconds.").isFalse();
	}

	protected void enableExclusiveMode()
	{
		taskEngineExclusiveModeSwitch.switchToValue("true");
	}

	protected void resetExclusiveMode()
	{
		taskEngineExclusiveModeSwitch.switchBackToDefault();
	}

	static class SpyTasksProvider implements TasksProvider
	{
		private final TasksProvider taskProvider;
		private int getTasksToScheduleInvocationCount = 0;

		private SpyTasksProvider(final TasksProvider tasksProvider)
		{
			this.taskProvider = tasksProvider;
		}

		static SpyTasksProvider of(final TasksProvider tasksProvider)
		{
			return new SpyTasksProvider(tasksProvider);
		}

		@Override
		public List<VersionPK> getTasksToSchedule(final RuntimeConfigHolder runtimeConfigHolder,
		                                          final TaskEngineParameters taskEngineParameters, final int maxItemsToSchedule)
		{
			getTasksToScheduleInvocationCount++;
			return taskProvider.getTasksToSchedule(runtimeConfigHolder, taskEngineParameters, maxItemsToSchedule);
		}

		@Override
		public void afterTaskFinished(final PK taskPk, final RuntimeConfigHolder runtimeConfigHolder)
		{
			taskProvider.afterTaskFinished(taskPk, runtimeConfigHolder);
		}

		@Override
		public void afterTaskUnlocked(final PK taskPk, final RuntimeConfigHolder runtimeConfigHolder)
		{
			taskProvider.afterTaskUnlocked(taskPk, runtimeConfigHolder);
		}

		@Override
		public void beforeTaskEngineStart(final int nodeId)
		{
			taskProvider.beforeTaskEngineStart(nodeId);
		}

		@Override
		public void afterTaskEngineStop(final int nodeId, final RuntimeConfigHolder runtimeConfigHolder)
		{
			taskProvider.afterTaskEngineStop(nodeId, runtimeConfigHolder);
		}

		@Override
		public int getMaxItemsToSchedule(final DefaultTaskService.TaskEngineRunningState runningState,
		                                 final RuntimeConfigHolder runtimeConfigHolder)
		{
			return taskProvider.getMaxItemsToSchedule(runningState, runtimeConfigHolder);
		}

		public int getTasksToScheduleInvocationCount()
		{
			return getTasksToScheduleInvocationCount;
		}
	}

	public class TestTaskService extends DefaultTaskService
	{

		private final int nodeId;
		private final Collection<String> nodeGroups;
		private final TestExecutionStrategy testExecutionStrategy;
		private final boolean isEclusive;
		private final SpyTasksProvider tasksProvider;
		private final boolean processTriggerTask;
		private final TaskRunner taskRunner;

		public TestTaskService(final int nodeId, final Collection<String> nodeGroups, final CountDownLatch countDownLatch,
		                       final CountDownLatch deleteCountDownLatch, final Collection<Long> relevantTasks)
		{
			this(nodeId, nodeGroups, countDownLatch, deleteCountDownLatch, relevantTasks, true);
		}

		public TestTaskService(
				final int nodeId, final Collection<String> nodeGroups, final CountDownLatch countDownLatch,
				final CountDownLatch deleteCountDownLatch, final Collection<Long> relevantTasks, final boolean processTriggerTask)
		{
			this(nodeId, nodeGroups, t -> relevantTasks.contains(t.getPk().getLong()), countDownLatch, deleteCountDownLatch,
					Config.getBoolean(TASK_ENGINE_EXCLUSIVE_MODE, false), processTriggerTask);
		}

		public TestTaskService(final int nodeId, final Collection<String> nodeGroups, final Predicate<TaskModel> shouldProcess,
		                       final CountDownLatch countDownLatch, final CountDownLatch deleteCountDownLatch,
		                       final boolean isExclusive)
		{
			this(nodeId, nodeGroups, shouldProcess, countDownLatch, deleteCountDownLatch, isExclusive, true);
		}

		public TestTaskService(
				final int nodeId, final Collection<String> nodeGroups, final Predicate<TaskModel> shouldProcess,
				final CountDownLatch countDownLatch, final CountDownLatch deleteCountDownLatch,
				final boolean isExclusive, final boolean processTriggerTask)
		{
			this(nodeId, nodeGroups, isExclusive, processTriggerTask,
					new TestExecutionStrategy(shouldProcess, countDownLatch, deleteCountDownLatch), null);
		}

		public TestTaskService(
				final int nodeId, final Collection<String> nodeGroups,
				final boolean isExclusive, final boolean processTriggerTask,
				final TestExecutionStrategy testExecutionStrategy, final TaskRunner taskRunner)
		{
			this.nodeId = nodeId;
			this.nodeGroups = nodeGroups;
			this.processTriggerTask = processTriggerTask;
			this.testExecutionStrategy = testExecutionStrategy;
			this.taskRunner = taskRunner;
			this.isEclusive = isExclusive;
			this.setTaskEngineRetryTemplate(taskEngineRetryTemplate);

			this.setModelService(modelService);
			this.setMetricRegistry(metricRegistry);
			this.setTaskExecutionStrategies(Lists.newArrayList(this.testExecutionStrategy));
			this.tasksProvider = SpyTasksProvider.of(getTasksProvider());
			this.setTasksProvider(this.tasksProvider);
			this.setTaskDao(new TaskDAO(getTenant())
			{
				@Override
				protected int initializeClusterId()
				{
					return nodeId;
				}
			});
		}

		@Override
		protected TaskExecutionStrategy getExecutionStrategy(final TaskRunner<? extends TaskModel> runner)
		{
			return testExecutionStrategy;
		}

		@Override
		protected TaskEngineParameters getTaskProviderParameters()
		{
			return new TaskEngineParameters.ParametersBuilder().clone(super.getTaskProviderParameters())
			                                                   .withExclusiveMode(isEclusive)
			                                                   .withProcessTriggerTask(processTriggerTask)
			                                                   .build();
		}

		@Override
		Collection<String> getClusterGroupsForThisNode()
		{
			return nodeGroups;
		}

		@Override
		int getClusterNodeID()
		{
			return nodeId;
		}

		@Override
		void scheduleTaskForExecution(final TasksProvider.VersionPK versionedPK)
		{
			super.scheduleTaskForExecution(versionedPK);
		}

		public SpyTasksProvider getSpyTaskProvider()
		{
			return tasksProvider;
		}

		public Collection<Long> getExecutedTasks()
		{
			return testExecutionStrategy.getExecutedTasks();
		}

		public Collection<Long> getFailedTasks()
		{
			return testExecutionStrategy.getFailedTasks();
		}

		@Override
		protected TaskRunner getRunner(final String runnerBean) throws IllegalStateException
		{
			return taskRunner;
		}

		public int getMaxItemsToSchedule()
		{
			return getTaskProviderParameters().getMaxItemsToSchedule();
		}
	}

	class TestExecutionStrategy implements TaskExecutionStrategy
	{

		private final Predicate<TaskModel> shouldProcess;
		private final Collection<Long> executedTasks = Collections.synchronizedList(new ArrayList<Long>());
		private final Collection<Long> failedTasks = Collections.synchronizedList(new ArrayList<Long>());
		private final CountDownLatch countDownLatch;
		private final CountDownLatch deletionCountDownLatch;
		private final TaskExecutionStrategy delegate;

		public TestExecutionStrategy(final Predicate<TaskModel> shouldProcess, final CountDownLatch countDownLatch,
		                             final CountDownLatch deletionCountDownLatch)
		{
			this(shouldProcess, countDownLatch, deletionCountDownLatch, null);
		}

		public TestExecutionStrategy(
				final Predicate<TaskModel> shouldProcess, final CountDownLatch countDownLatch,
				final CountDownLatch deletionCountDownLatch, final TaskExecutionStrategy delegate)
		{
			this.shouldProcess = shouldProcess;
			this.countDownLatch = countDownLatch;
			this.deletionCountDownLatch = deletionCountDownLatch;
			this.delegate = delegate;
		}

		@Override
		public void run(final TaskService taskService, final TaskRunner<TaskModel> runner, final TaskModel model)
				throws RetryLaterException
		{
			final Long pk = model.getPk().getLong();
			if (shouldProcess.test(model))
			{
				executedTasks.add(pk);
				countDownLatch.countDown();
			}
			if (delegate != null)
			{
				delegate.run(taskService, runner, model);
			}
		}

		@Override
		public Throwable handleError(final TaskService taskService, final TaskRunner<TaskModel> runner, final TaskModel model,
		                             final Throwable error)
		{
			final Long pk = model.getPk().getLong();
			if (shouldProcess.test(model))
			{
				failedTasks.add(pk);
				countDownLatch.countDown();
			}
			if (delegate != null)
			{
				return delegate.handleError(taskService, runner, model, error);
			}
			return null;
		}

		@Override
		public void finished(final TaskService taskService, final TaskRunner<TaskModel> runner, final TaskModel model,
		                     final Throwable error)
		{
			modelService.remove(model);
			deletionCountDownLatch.countDown();
			if (delegate != null)
			{
				delegate.finished(taskService, runner, model, error);
			}
		}

		@Override
		public Date handleRetry(final TaskService taskService, final TaskRunner<TaskModel> runner, final TaskModel model,
		                        final RetryLaterException retry, final int currentRetries)
		{
			if (delegate != null)
			{
				return delegate.handleRetry(taskService, runner, model, retry, currentRetries);
			}
			return null;
		}

		@Override
		public Class<? extends TaskRunner<? extends TaskModel>> getRunnerClass()
		{
			if (delegate != null)
			{
				return delegate.getRunnerClass();
			}
			return TestTaskRunner.class;
		}

		public Collection<Long> getExecutedTasks()
		{
			return executedTasks;
		}

		public Collection<Long> getFailedTasks()
		{
			return failedTasks;
		}
	}
}
