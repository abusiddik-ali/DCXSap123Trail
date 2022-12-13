/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.PK;

import org.apache.commons.lang3.RandomUtils;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@UnitTest
public class DelegatingTasksProviderTest
{

	@Mock
	private TasksProvider tasksProvider;

	private DelegatingTasksProvider delegatingTasksProvider;

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);

		delegatingTasksProvider = new DelegatingTasksProvider(tasksProvider)
		{
		};

	}

	@Test
	public void testGetTasksToSchedule()
	{
		final RuntimeConfigHolder runtimeConfigHolder = mock(MockRuntimeConfigHolder.class);
		final TaskEngineParameters taskEngineParameters = mock(TaskEngineParameters.class);
		final int itemsToSchedule = RandomUtils.nextInt(0, 1000);

		delegatingTasksProvider.getTasksToSchedule(runtimeConfigHolder, taskEngineParameters, itemsToSchedule);

		verify(tasksProvider).getTasksToSchedule(eq(runtimeConfigHolder), eq(taskEngineParameters), eq(itemsToSchedule));
	}

	@Test
	public void testAfterTaskFinished()
	{
		final RuntimeConfigHolder runtimeConfigHolder = mock(MockRuntimeConfigHolder.class);
		final PK pk = PK.fromLong(RandomUtils.nextLong());

		delegatingTasksProvider.afterTaskFinished(pk, runtimeConfigHolder);

		verify(tasksProvider).afterTaskFinished(eq(pk), eq(runtimeConfigHolder));
	}

	@Test
	public void testAfterTaskUnlocked()
	{
		final RuntimeConfigHolder runtimeConfigHolder = mock(MockRuntimeConfigHolder.class);
		final PK pk = PK.fromLong(RandomUtils.nextLong());

		delegatingTasksProvider.afterTaskUnlocked(pk, runtimeConfigHolder);

		verify(tasksProvider).afterTaskUnlocked(eq(pk), eq(runtimeConfigHolder));
	}

	@Test
	public void testBeforeTaskEngineStart()
	{
		final int nodeId = RandomUtils.nextInt(0, 1000);

		delegatingTasksProvider.beforeTaskEngineStart(nodeId);

		verify(tasksProvider).beforeTaskEngineStart(eq(nodeId));
	}

	@Test
	public void testAfterTaskEngineStop()
	{
		final RuntimeConfigHolder runtimeConfigHolder = mock(MockRuntimeConfigHolder.class);
		final int nodeId = RandomUtils.nextInt(0, 1000);

		delegatingTasksProvider.afterTaskEngineStop(nodeId, runtimeConfigHolder);

		verify(tasksProvider).afterTaskEngineStop(eq(nodeId), eq(runtimeConfigHolder));
	}

	@Test
	public void testGetMaxItemsToSchedule()
	{
		final RuntimeConfigHolder runtimeConfigHolder = mock(MockRuntimeConfigHolder.class);
		final DefaultTaskService.TaskEngineRunningState taskEngineRunningState = mock(MockTaskEngineRunningState.class);

		delegatingTasksProvider.getMaxItemsToSchedule(taskEngineRunningState, runtimeConfigHolder);

		verify(tasksProvider).getMaxItemsToSchedule(eq(taskEngineRunningState), eq(runtimeConfigHolder));
	}

	@Test
	public void testGetTasksProvider()
	{
		final TasksProvider tasksProvider = delegatingTasksProvider.getTasksProvider();

		assertThat(tasksProvider).isEqualTo(this.tasksProvider);
	}

	public static class MockRuntimeConfigHolder extends RuntimeConfigHolder
	{
	}

	public static class MockTaskEngineRunningState extends DefaultTaskService.TaskEngineRunningState
	{

		MockTaskEngineRunningState()
		{
			super(null, null);
		}
	}
}
