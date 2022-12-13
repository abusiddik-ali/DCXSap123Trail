/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;

import static de.hybris.platform.task.impl.RuntimeConfigHolder.intProperty;

import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.task.impl.RuntimeConfigHolder.IntTaskEngineProperty;

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@UnitTest
public class AdjustItemsToScheduleCountTasksProviderTest
{
	private final IntTaskEngineProperty multiplierProperty = intProperty(UUID.randomUUID().toString(), 10);

	private AdjustItemsToScheduleCountTasksProvider provider;

	@Mock
	private TasksProvider mockDelegate;

	@Mock
	private MockRuntimeConfigHolder mockConfigHolder;

	private ArgumentCaptor<Integer> mockDelegateMaxItemsToScheduleCaptor;

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);

		provider = new AdjustItemsToScheduleCountTasksProvider(mockDelegate, multiplierProperty);

		mockDelegateMaxItemsToScheduleCaptor = ArgumentCaptor.forClass(Integer.class);
		when(mockDelegate.getTasksToSchedule(any(), any(), mockDelegateMaxItemsToScheduleCaptor.capture())).thenReturn(
				emptyList());
	}

	@Test
	public void shouldMultiplyTheNumberOfItemsToPoll()
	{
		final int itemsToSchedule = 10;
		final int multiplier = 20;

		when(mockConfigHolder.getProperty(multiplierProperty)).thenReturn(multiplier);

		provider.getTasksToSchedule(mockConfigHolder, null, itemsToSchedule);

		assertThat(mockDelegateMaxItemsToScheduleCaptor.getAllValues()).hasSize(1);
		assertThat(mockDelegateMaxItemsToScheduleCaptor.getValue()).isEqualTo(itemsToSchedule * multiplier);
	}

	@Test
	public void shouldMultiplyTheNumberOfItemsToPollWithNegativeMultiplier()
	{
		final int itemsToSchedule = 10;
		final int multiplier = -1;

		when(mockConfigHolder.getProperty(multiplierProperty)).thenReturn(multiplier);

		provider.getTasksToSchedule(mockConfigHolder, null, itemsToSchedule);

		assertThat(mockDelegateMaxItemsToScheduleCaptor.getAllValues()).hasSize(1);
		assertThat(mockDelegateMaxItemsToScheduleCaptor.getValue()).isEqualTo(itemsToSchedule);
	}

	@Test
	public void shouldMultiplyTheNumberOfItemsToPollWithZeroMultiplier()
	{
		final int itemsToSchedule = 10;
		final int multiplier = 0;

		when(mockConfigHolder.getProperty(multiplierProperty)).thenReturn(multiplier);

		provider.getTasksToSchedule(mockConfigHolder, null, itemsToSchedule);

		assertThat(mockDelegateMaxItemsToScheduleCaptor.getAllValues()).hasSize(1);
		assertThat(mockDelegateMaxItemsToScheduleCaptor.getValue()).isEqualTo(itemsToSchedule);
	}

	public static class MockRuntimeConfigHolder extends RuntimeConfigHolder
	{
		@Override
		public <T> T getProperty(final TaskEngineProperty<T> parameter)
		{
			return super.getProperty(parameter);
		}
	}
}
