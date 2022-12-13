/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.task.constants.TaskConstants;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.time.Duration;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.assertj.core.data.Offset;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.google.common.base.Stopwatch;
import com.google.common.collect.Lists;

public abstract class DefaultTaskServiceRepollTest extends DefaultTaskServiceBaseTest
{
	private final PropertyConfigSwitcher taskPollInterval = new PropertyConfigSwitcher(TaskConstants.Params.POLLING_INTERVAL);
	private static final Integer POLL_INTERVAL = 2;

	private static final Duration LATCH_WAIT_TIMEOUT = Duration.ofMinutes(2);

	@Override
	@Before
	public void setUp()
	{
		super.setUp();
		taskPollInterval.switchToValue(POLL_INTERVAL.toString());
	}

	@Override
	@After
	public void tearDown()
	{
		super.tearDown();
		taskPollInterval.switchBackToDefault();
	}

	@Test
	public void shouldExecuteTasksOnlyOnNonExclusiveNodeGroup() throws Exception
	{
		assureTaskEngineStopped();

		final String group_a = "group_a";
		final String group_b = "group_b";
		final int taskCount = 160;

		final CountDownLatch latch = new CountDownLatch(taskCount);
		final CountDownLatch deleteLatch = new CountDownLatch(taskCount);

		final TestTaskService serviceA = new TestTaskService(0, Lists.newArrayList(group_a),
				t -> t.getContext().equals("shouldExecuteTasksOnlyOnNonExclusiveNodeGroup-testContext"), latch, deleteLatch, false);
		final TestTaskService serviceB = new TestTaskService(1, Lists.newArrayList(group_b),
				t -> t.getContext().equals("shouldExecuteTasksOnlyOnNonExclusiveNodeGroup-testContext"), latch, deleteLatch, true);

		try
		{
			serviceB.getEngine().start();
			Thread.sleep(TimeUnit.SECONDS.toMillis(2));
			serviceA.getEngine().start();
			final Stopwatch timer = Stopwatch.createStarted();

			int i = 0;
			while (i++ < taskCount)
			{
				createTask(null, t -> {
					t.setContext("shouldExecuteTasksOnlyOnNonExclusiveNodeGroup-testContext");
					return t;
				});
			}
			assertThat(latch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();

			final int timesExecutedA = serviceA.getSpyTaskProvider().getTasksToScheduleInvocationCount();
			final int timesExecutedB = serviceB.getSpyTaskProvider().getTasksToScheduleInvocationCount();

			final int expectedExecCountB = (int) (timer.stop().elapsed().getSeconds() / POLL_INTERVAL) + 3;

			assertThat(timesExecutedB).isLessThanOrEqualTo(expectedExecCountB);
			assertThat(timesExecutedB).isLessThanOrEqualTo(timesExecutedA);

			assertThat(serviceA.getExecutedTasks().size()).isEqualTo(taskCount);
			assertThat(serviceB.getExecutedTasks().size()).isEqualTo(0);

			assertThat(deleteLatch.await(LATCH_WAIT_TIMEOUT.toSeconds(), TimeUnit.SECONDS)).isTrue();
		}
		finally
		{
			serviceB.getEngine().stop();
			serviceA.getEngine().stop();
		}
	}
}
