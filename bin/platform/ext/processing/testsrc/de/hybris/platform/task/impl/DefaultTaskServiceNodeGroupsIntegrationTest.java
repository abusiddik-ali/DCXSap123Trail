/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;


import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.cronjob.model.TriggerModel;
import de.hybris.platform.servicelayer.internal.model.ScriptingJobModel;
import de.hybris.platform.task.model.TriggerTaskModel;

import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.Test;

import com.google.common.collect.Lists;


public abstract class DefaultTaskServiceNodeGroupsIntegrationTest extends DefaultTaskServiceBaseTest
{

	private static final Duration LATCH_WAIT_TIMEOUT = Duration.ofMinutes(2);

	private CountDownLatch countDownLatch;
	private CountDownLatch deletionCountDownLatch;

	@Test
	public void shouldExecuteOnlyTasksForOwnNodeGroup() throws Exception
	{
		assureTaskEngineStopped();

		final String group_a = "group_a";
		final String group_b = "group_b";

		final Long tA1 = createTask(group_a);
		final Long tA2 = createTask(group_a);
		final Long tA3 = createTask(group_a);

		final Long tB1 = createTask(group_b);
		final Long tB2 = createTask(group_b);
		final Long tB3 = createTask(group_b);
		final Long tB4 = createTask(group_b);

		final Long tNone1 = createTask(null);
		final Long tNone2 = createTask(null);
		final Long tNone3 = createTask(null);

		final Collection<Long> relevantTasks = Lists.newArrayList(tA1, tA2, tA3, tB1, tB2, tB3, tB4, tNone1, tNone2, tNone3);

		countDownLatch = new CountDownLatch(relevantTasks.size());
		deletionCountDownLatch = new CountDownLatch(relevantTasks.size());

		final TestTaskService serviceA = new TestTaskService(0, Lists.newArrayList(group_a), countDownLatch,
				deletionCountDownLatch, relevantTasks);
		final TestTaskService serviceB = new TestTaskService(1, Lists.newArrayList(group_b), countDownLatch,
				deletionCountDownLatch, relevantTasks);

		try
		{
			serviceB.getEngine().start();
			serviceA.getEngine().start();

			assertThat(countDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();

			assertThat(serviceA.getExecutedTasks()).contains(tA1, tA2, tA3).doesNotContain(tB1, tB2, tB3, tB4);
			assertThat(serviceB.getExecutedTasks()).contains(tB1, tB2, tB3, tB4).doesNotContain(tA1, tA2, tA3);

			final List<Long> allExecutedTasks = Lists.newArrayList(serviceA.getExecutedTasks());
			allExecutedTasks.addAll(serviceB.getExecutedTasks());

			assertThat(allExecutedTasks).containsOnlyOnce(relevantTasks.toArray(new Long[0]));

			assertThat(deletionCountDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();
		}
		finally
		{
			serviceB.getEngine().stop();
			serviceA.getEngine().stop();
		}
	}

	@Test
	public void shouldExecuteOnlyTasksForOwnNodeGroupInExclusiveMode() throws Exception
	{
		assureTaskEngineStopped();
		enableExclusiveMode();

		final String group_a = "group_a";
		final String group_b = "group_b";

		final Long tA1 = createTask(group_a);
		final Long tA2 = createTask(group_a);
		final Long tA3 = createTask(group_a);

		final Long tB1 = createTask(group_b);
		final Long tB2 = createTask(group_b);
		final Long tB3 = createTask(group_b);
		final Long tB4 = createTask(group_b);

		final Long tNone1 = createTask(null);
		final Long tNone2 = createTask(null);
		final Long tNone3 = createTask(null);

		final Collection<Long> relevantTasks = Lists.newArrayList(tA1, tA2, tA3, tB1, tB2, tB3, tB4, tNone1, tNone2, tNone3);

		countDownLatch = new CountDownLatch(relevantTasks.size() - 3);
		deletionCountDownLatch = new CountDownLatch(relevantTasks.size() - 3);

		final TestTaskService serviceA = new TestTaskService(0, Lists.newArrayList(group_a), countDownLatch,
				deletionCountDownLatch, relevantTasks);
		final TestTaskService serviceB = new TestTaskService(1, Lists.newArrayList(group_b), countDownLatch,
				deletionCountDownLatch, relevantTasks);


		try
		{
			serviceB.getEngine().start();
			serviceA.getEngine().start();

			assertThat(countDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();

			assertThat(serviceA.getExecutedTasks()).containsOnly(tA1, tA2, tA3);
			assertThat(serviceB.getExecutedTasks()).containsOnly(tB1, tB2, tB3, tB4);

			assertThat(deletionCountDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();
		}
		finally
		{
			serviceB.getEngine().stop();
			serviceA.getEngine().stop();
		}
	}

	@Test
	public void shouldFailAllExpiredTasks() throws Exception
	{
		assureTaskEngineStopped();

		final String group_a = "group_a";
		final String group_b = "group_b";

		//expired tasks
		final Long tA1ex = createExpiredTask(group_a);
		final Long tB1ex = createExpiredTask(group_b);
		final Long tNone1ex = createExpiredTask(null);

		final Collection<Long> relevantTasks = Lists.newArrayList(tA1ex, tB1ex, tNone1ex);

		countDownLatch = new CountDownLatch(relevantTasks.size());
		deletionCountDownLatch = new CountDownLatch(relevantTasks.size());

		final TestTaskService serviceA = new TestTaskService(0, Lists.newArrayList(group_a), countDownLatch,
				deletionCountDownLatch, relevantTasks);
		final TestTaskService serviceB = new TestTaskService(1, Lists.newArrayList(group_b), countDownLatch,
				deletionCountDownLatch, relevantTasks);

		try
		{
			serviceB.getEngine().start();
			serviceA.getEngine().start();

			assertThat(countDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();

			assertThat(serviceA.getExecutedTasks()).isEmpty();
			assertThat(serviceB.getExecutedTasks()).isEmpty();

			final List<Long> failedTasks = Lists.newArrayList(serviceA.getFailedTasks());
			failedTasks.addAll(serviceB.getFailedTasks());
			assertThat(failedTasks).containsOnlyOnce(relevantTasks.toArray(new Long[0]));

			assertThat(deletionCountDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();
		}
		finally
		{
			serviceB.getEngine().stop();
			serviceA.getEngine().stop();
		}
	}


	@Test
	public void shouldFailOnlyExpiredTasksForOwnNodeGroupInExclusiveMode() throws Exception
	{
		assureTaskEngineStopped();
		enableExclusiveMode();

		final String group_a = "group_a";
		final String group_b = "group_b";

		//expired tasks
		final Long tA1ex = createExpiredTask(group_a);
		final Long tB1ex = createExpiredTask(group_b);
		final Long tNone1ex = createExpiredTask(null);

		final Collection<Long> relevantTasks = Lists.newArrayList(tA1ex, tB1ex, tNone1ex);

		countDownLatch = new CountDownLatch(relevantTasks.size() - 1);
		deletionCountDownLatch = new CountDownLatch(relevantTasks.size() - 1);

		final TestTaskService serviceA = new TestTaskService(0, Lists.newArrayList(group_a), countDownLatch,
				deletionCountDownLatch, relevantTasks);
		final TestTaskService serviceB = new TestTaskService(1, Lists.newArrayList(group_b), countDownLatch,
				deletionCountDownLatch, relevantTasks);

		try
		{
			serviceB.getEngine().start();
			serviceA.getEngine().start();

			assertThat(countDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();

			assertThat(serviceA.getExecutedTasks()).isEmpty();
			assertThat(serviceB.getExecutedTasks()).isEmpty();

			assertThat(serviceA.getFailedTasks()).contains(tA1ex);
			assertThat(serviceB.getFailedTasks()).contains(tB1ex);

			assertThat(deletionCountDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();
		}
		finally
		{
			serviceB.getEngine().stop();
			serviceA.getEngine().stop();
		}
	}


	@Test
	public void shouldExecuteOnlyTasksForOwnNode() throws Exception
	{
		assureTaskEngineStopped();

		final String group_a = "group_a";
		final String group_b = "group_b";

		final Long tA1 = createTask(0);
		final Long tA2 = createTask(0);
		final Long tA3 = createTask(0);

		final Long tB1 = createTask(1);
		final Long tB2 = createTask(1);
		final Long tB3 = createTask(1);
		final Long tB4 = createTask(1);

		final Long tNone1 = createTask(null);
		final Long tNone2 = createTask(null);
		final Long tNone3 = createTask(null);

		final Collection<Long> relevantTasks = Lists.newArrayList(tA1, tA2, tA3, tB1, tB2, tB3, tB4, tNone1, tNone2, tNone3);

		countDownLatch = new CountDownLatch(relevantTasks.size());
		deletionCountDownLatch = new CountDownLatch(relevantTasks.size());

		final TestTaskService serviceA = new TestTaskService(0, Lists.newArrayList(group_a), countDownLatch,
				deletionCountDownLatch, relevantTasks);
		final TestTaskService serviceB = new TestTaskService(1, Lists.newArrayList(group_b), countDownLatch,
				deletionCountDownLatch, relevantTasks);

		try
		{
			serviceB.getEngine().start();
			serviceA.getEngine().start();

			assertThat(countDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();

			assertThat(serviceA.getExecutedTasks()).contains(tA1, tA2, tA3).doesNotContain(tB1, tB2, tB3, tB4);
			assertThat(serviceB.getExecutedTasks()).contains(tB1, tB2, tB3, tB4).doesNotContain(tA1, tA2, tA3);

			final List<Long> allExecutedTasks = Lists.newArrayList(serviceA.getExecutedTasks());
			allExecutedTasks.addAll(serviceB.getExecutedTasks());

			assertThat(allExecutedTasks).containsOnlyOnce(relevantTasks.toArray(new Long[0]));

			assertThat(deletionCountDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();
		}
		finally
		{
			serviceB.getEngine().stop();
			serviceA.getEngine().stop();
		}
	}

	@Test
	public void shouldFailOnlyExpiredTasksForOwnNodeIdInExclusiveMode() throws Exception
	{
		assureTaskEngineStopped();
		enableExclusiveMode();

		final String group_a = "group_a";
		final String group_b = "group_b";

		//expired tasks
		final Long tA1ex = createExpiredTask(0);
		final Long tB1ex = createExpiredTask(1);
		final Long tNone1ex = createExpiredTask(null);

		final Collection<Long> relevantTasks = Lists.newArrayList(tA1ex, tB1ex, tNone1ex);

		countDownLatch = new CountDownLatch(relevantTasks.size() - 1);
		deletionCountDownLatch = new CountDownLatch(relevantTasks.size() - 1);

		final TestTaskService serviceA = new TestTaskService(0, Lists.newArrayList(group_a), countDownLatch,
				deletionCountDownLatch, relevantTasks);
		final TestTaskService serviceB = new TestTaskService(1, Lists.newArrayList(group_b), countDownLatch,
				deletionCountDownLatch, relevantTasks);

		try
		{
			serviceB.getEngine().start();
			serviceA.getEngine().start();

			assertThat(countDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();

			assertThat(serviceA.getExecutedTasks()).isEmpty();
			assertThat(serviceB.getExecutedTasks()).isEmpty();

			assertThat(serviceA.getFailedTasks()).contains(tA1ex).doesNotContain(tB1ex);
			assertThat(serviceB.getFailedTasks()).contains(tB1ex).doesNotContain(tA1ex);

			assertThat(deletionCountDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();
		}
		finally
		{
			serviceB.getEngine().stop();
			serviceA.getEngine().stop();
		}
	}


	@Test
	public void shouldExecuteOnlyTasksForOwnNodeWhenNoGroupDefined() throws Exception
	{
		assureTaskEngineStopped();

		final Long tA1 = createTask(0);
		final Long tA2 = createTask(0);
		final Long tA3 = createTask(0);

		final Long tB1 = createTask(1);
		final Long tB2 = createTask(1);
		final Long tB3 = createTask(1);
		final Long tB4 = createTask(1);

		final Long tNone1 = createTask(null);
		final Long tNone2 = createTask(null);
		final Long tNone3 = createTask(null);

		final Collection<Long> relevantTasks = Lists.newArrayList(tA1, tA2, tA3, tB1, tB2, tB3, tB4, tNone1, tNone2, tNone3);

		countDownLatch = new CountDownLatch(relevantTasks.size());
		deletionCountDownLatch = new CountDownLatch(relevantTasks.size());

		final TestTaskService serviceA = new TestTaskService(0, Collections.emptyList(), countDownLatch, deletionCountDownLatch,
				relevantTasks);
		final TestTaskService serviceB = new TestTaskService(1, Collections.emptyList(), countDownLatch, deletionCountDownLatch,
				relevantTasks);

		try
		{
			serviceB.getEngine().start();
			serviceA.getEngine().start();

			assertThat(countDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();

			assertThat(serviceA.getExecutedTasks()).contains(tA1, tA2, tA3).doesNotContain(tB1, tB2, tB3, tB4);
			assertThat(serviceB.getExecutedTasks()).contains(tB1, tB2, tB3, tB4).doesNotContain(tA1, tA2, tA3);

			final List<Long> allExecutedTasks = Lists.newArrayList(serviceA.getExecutedTasks());
			allExecutedTasks.addAll(serviceB.getExecutedTasks());

			assertThat(allExecutedTasks).containsOnlyOnce(relevantTasks.toArray(new Long[0]));

			assertThat(deletionCountDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();
		}
		finally
		{
			serviceB.getEngine().stop();
			serviceA.getEngine().stop();
		}
	}


	@Test
	public void shouldExecuteOnlyTasksForOwnNodeInExclusiveMode() throws Exception
	{
		assureTaskEngineStopped();
		enableExclusiveMode();

		final String group_a = "group_a";
		final String group_b = "group_b";

		final Long tA1 = createTask(0);
		final Long tA2 = createTask(0);
		final Long tA3 = createTask(0);

		final Long tB1 = createTask(1);
		final Long tB2 = createTask(1);
		final Long tB3 = createTask(1);
		final Long tB4 = createTask(1);

		final Long tNone1 = createTask(null);
		final Long tNone2 = createTask(null);
		final Long tNone3 = createTask(null);

		final Collection<Long> relevantTasks = Lists.newArrayList(tA1, tA2, tA3, tB1, tB2, tB3, tB4, tNone1, tNone2, tNone3);

		countDownLatch = new CountDownLatch(relevantTasks.size() - 3);
		deletionCountDownLatch = new CountDownLatch(relevantTasks.size() - 3);

		final TestTaskService serviceA = new TestTaskService(0, Lists.newArrayList(group_a), countDownLatch,
				deletionCountDownLatch, relevantTasks);
		final TestTaskService serviceB = new TestTaskService(1, Lists.newArrayList(group_b), countDownLatch,
				deletionCountDownLatch, relevantTasks);

		try
		{
			serviceB.getEngine().start();
			serviceA.getEngine().start();

			assertThat(countDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();

			assertThat(serviceA.getExecutedTasks()).containsOnly(tA1, tA2, tA3);
			assertThat(serviceB.getExecutedTasks()).containsOnly(tB1, tB2, tB3, tB4);

			assertThat(deletionCountDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();
		}
		finally
		{
			serviceB.getEngine().stop();
			serviceA.getEngine().stop();
		}
	}

	@Test
	public void shouldExecuteTasksWithConflictingNodeIfAndNodeGroupAssignment() throws Exception
	{
		assureTaskEngineStopped();

		final String group_a = "group_a";
		final String group_b = "group_b";

		final Long tA1 = createTask(0, group_b);
		final Long tA2 = createTask(0, group_b);
		final Long tA3 = createTask(0, group_b);

		final Long tB1 = createTask(1, group_a);
		final Long tB2 = createTask(1, group_a);
		final Long tB3 = createTask(1, group_a);
		final Long tB4 = createTask(1, group_a);

		final Long tNone1 = createTask();
		final Long tNone2 = createTask();
		final Long tNone3 = createTask();

		final Collection<Long> relevantTasks = Lists.newArrayList(tA1, tA2, tA3, tB1, tB2, tB3, tB4, tNone1, tNone2, tNone3);

		countDownLatch = new CountDownLatch(relevantTasks.size());
		deletionCountDownLatch = new CountDownLatch(relevantTasks.size());

		final TestTaskService serviceA = new TestTaskService(0, Lists.newArrayList(group_a), countDownLatch,
				deletionCountDownLatch, relevantTasks);
		final TestTaskService serviceB = new TestTaskService(1, Lists.newArrayList(group_b), countDownLatch,
				deletionCountDownLatch, relevantTasks);

		try
		{
			serviceB.getEngine().start();
			serviceA.getEngine().start();

			assertThat(countDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();

			assertThat(serviceA.getExecutedTasks()).contains(tA1, tA2, tA3).doesNotContain(tB1, tB2, tB3, tB4);
			assertThat(serviceB.getExecutedTasks()).contains(tB1, tB2, tB3, tB4).doesNotContain(tA1, tA2, tA3);

			final List<Long> allExecutedTasks = Lists.newArrayList(serviceA.getExecutedTasks());
			allExecutedTasks.addAll(serviceB.getExecutedTasks());

			assertThat(allExecutedTasks).containsOnlyOnce(relevantTasks.toArray(new Long[0]));

			assertThat(deletionCountDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();
		}
		finally
		{
			serviceB.getEngine().stop();
			serviceA.getEngine().stop();
		}
	}


	@Test
	public void shouldExecuteTaskWithoutSpecifiedNodeGroupByAnyNode() throws Exception
	{
		assureTaskEngineStopped();

		final String group_a = "group_a";
		final String group_b = "group_b";

		final Long t1 = createTask(null);
		final Long t2 = createTask(null);
		final Long t3 = createTask(null);
		final Long t4 = createTask(null);

		final Collection<Long> relevantTasks = Lists.newArrayList(t1, t2, t3, t4);

		countDownLatch = new CountDownLatch(relevantTasks.size());
		deletionCountDownLatch = new CountDownLatch(relevantTasks.size());

		final TestTaskService serviceA = new TestTaskService(0, Lists.newArrayList(group_a), countDownLatch,
				deletionCountDownLatch, relevantTasks);
		final TestTaskService serviceB = new TestTaskService(1, Lists.newArrayList(group_b), countDownLatch,
				deletionCountDownLatch, relevantTasks);

		try
		{
			serviceB.getEngine().start();
			serviceA.getEngine().start();

			assertThat(countDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();

			final Collection<Long> allExecutedTasks = new ArrayList<>();

			allExecutedTasks.addAll(serviceA.getExecutedTasks());
			allExecutedTasks.addAll(serviceB.getExecutedTasks());

			assertThat(allExecutedTasks).hasSize(4);
			assertThat(allExecutedTasks).containsOnly(t1, t2, t3, t4);

			assertThat(deletionCountDownLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();
		}
		finally
		{
			serviceB.getEngine().stop();
			serviceA.getEngine().stop();
		}
	}

	protected List<Long> createTriggerTasks(final int count)
	{

		final ScriptingJobModel job = modelService.create(ScriptingJobModel.class);
		job.setScriptURI("model://fake.uri");
		job.setCode("testJob");
		modelService.save(job);

		final CronJobModel cronJob = modelService.create(CronJobModel.class);
		cronJob.setCode("testCronJob");
		cronJob.setJob(job);

		final TriggerModel trigger = modelService.create(TriggerModel.class);
		trigger.setCronJob(cronJob);
		trigger.setActivationTime(Date.from(Instant.now().plus(30, ChronoUnit.DAYS)));

		modelService.saveAll();

		return IntStream.range(0, count).mapToObj(i -> {
					final TriggerTaskModel task = modelService.create(TriggerTaskModel.class);
					task.setRunnerBean("runner");
					task.setTrigger(trigger);
					taskService.scheduleTask(task);
					markTaskForRemovalAfterTest(task.getPk());
					return task.getPk().getLong();
				}
		).collect(Collectors.toList());
	}

	@Test
	public void shouldNotProcessTriggerTasksWhenItIsDisabledByConfiguration() throws Exception
	{
		assureTaskEngineStopped();

		final Collection<Long> relevantTasks = createTriggerTasks(3);

		countDownLatch = new CountDownLatch(relevantTasks.size());
		deletionCountDownLatch = new CountDownLatch(relevantTasks.size());

		final TestTaskService serviceA = new TestTaskService(0, List.of(), countDownLatch, deletionCountDownLatch, relevantTasks,
				false);

		try
		{
			serviceA.getEngine().start();

			countDownLatch.await(40, TimeUnit.SECONDS);

			assertThat(serviceA.getSpyTaskProvider().getTasksToScheduleInvocationCount()).isGreaterThan(0);
			assertThat(serviceA.getExecutedTasks()).isEmpty();
			assertThat(serviceA.getFailedTasks()).isEmpty();
		}
		finally
		{
			serviceA.getEngine().stop();
		}
	}
}
