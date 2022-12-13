/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;


import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;

import de.hybris.bootstrap.annotations.UnitTest;

import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
@UnitTest
public class AuxiliaryTablesBasedTaskProviderTest
{
	private static final int MAX_THREADS = 2;

	private final TasksProviderTest tasksProviderTest = new TasksProviderTest();

	@InjectMocks
	AuxiliaryTablesBasedTaskProvider auxiliaryTablesBasedTaskProvider;

	@Mock
	TaskEngineRunningStateTest runningState;

	@Mock
	RuntimeConfigHolderTest runtimeConfigHolder;
	private int defaultMaxItemsToSchedule;

	@Before
	public void setUp()
	{
		given(runningState.getMaxThreads()).willReturn(MAX_THREADS);
		defaultMaxItemsToSchedule = tasksProviderTest.getMaxItemsToSchedule(runningState, runtimeConfigHolder);
	}

	@Test
	public void shouldReturnMaxItemsToScheduleWithMultiplierGreaterThanOne()
	{
		final int multiplier = 5;
		final int itemsToSchedule = defaultMaxItemsToSchedule * multiplier;
		setMaxItemsMultiplier(multiplier);
		final int maxItemsToSchedule = auxiliaryTablesBasedTaskProvider.getMaxItemsToSchedule(runningState, runtimeConfigHolder);

		assertThat(itemsToSchedule).isEqualTo(maxItemsToSchedule);
	}

	@Test
	public void shouldReturnMaxItemsToScheduleWithMultiplierEqualToZero()
	{
		final int multiplier = 0;
		setMaxItemsMultiplier(multiplier);
		final int maxItemsToSchedule = auxiliaryTablesBasedTaskProvider.getMaxItemsToSchedule(runningState, runtimeConfigHolder);

		assertThat(defaultMaxItemsToSchedule).isEqualTo(maxItemsToSchedule);
	}

	@Test
	public void shouldReturnMaxItemsToScheduleWithMultiplierEqualToOne()
	{
		final int multiplier = 1;
		setMaxItemsMultiplier(multiplier);
		final int maxItemsToSchedule = auxiliaryTablesBasedTaskProvider.getMaxItemsToSchedule(runningState, runtimeConfigHolder);

		assertThat(defaultMaxItemsToSchedule).isEqualTo(maxItemsToSchedule);
	}

	@Test
	public void shouldReturnMaxItemsToScheduleWithNegativeMultiplier()
	{
		final int multiplier = -5;
		setMaxItemsMultiplier(multiplier);
		final int maxItemsToSchedule = auxiliaryTablesBasedTaskProvider.getMaxItemsToSchedule(runningState, runtimeConfigHolder);

		assertThat(defaultMaxItemsToSchedule).isEqualTo(maxItemsToSchedule);
	}

	private void setMaxItemsMultiplier(final int multiplier)
	{
		given(runtimeConfigHolder.getProperty(AuxiliaryTablesBasedTaskProvider.Params.WORKER_TASKS_COUNT_MULTIPLIER)).willReturn(
				multiplier);
	}

	public static class TaskEngineRunningStateTest extends DefaultTaskService.TaskEngineRunningState
	{
		TaskEngineRunningStateTest()
		{
			super(null, null);
		}

		@Override
		public int getMaxThreads()
		{
			return super.getMaxThreads();
		}
	}

	public static class RuntimeConfigHolderTest extends RuntimeConfigHolder
	{
		@Override
		public <T> T getProperty(final TaskEngineProperty<T> parameter)
		{
			return super.getProperty(parameter);
		}
	}

	public static class TasksProviderTest implements TasksProvider
	{
		@Override
		public List<VersionPK> getTasksToSchedule(final RuntimeConfigHolder runtimeConfigHolder,
		                                          final TaskEngineParameters taskEngineParameters, final int maxItemsToSchedule)
		{
			return null;
		}
	}
}
