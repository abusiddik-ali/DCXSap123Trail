/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.task.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.task.impl.gateways.TasksQueueGateway;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

import org.apache.commons.lang3.RandomUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import com.codahale.metrics.MetricRegistry;
import com.google.common.collect.ImmutableSet;

@RunWith(MockitoJUnitRunner.class)
@UnitTest
public class AuxiliaryTablesWorkerRoleTest
{
	private static final int DELETE_BATCH_SIZE = 10;

	@Mock
	private MetricRegistry metricRegistry;
	@Mock
	private AuxiliaryTablesGatewayFactory gatewayFactory;

	@Mock
	private TasksQueueGateway tasksQueueGateway;

	@Mock
	private MockRuntimeConfigHolder runtimeConfigHolder;

	@Captor
	private ArgumentCaptor<List<PK>> pksArgumentCaptor;

	@Before
	public void setUp() throws Exception
	{
		when(gatewayFactory.getTasksQueueGateway()).thenReturn(tasksQueueGateway);

		when(runtimeConfigHolder.getProperty(
				AuxiliaryTablesBasedTaskProvider.Params.WORKER_DELETE_TASKS_MAX_BATCH_SIZE)).thenReturn(DELETE_BATCH_SIZE);
	}

	@Test
	public void shouldForceDeleteWhenCallingDeleteTasks()
	{
		final AuxiliaryTablesWorkerRole workerRole = getAuxiliaryTablesWorkerRole();


		final PK pk1 = PK.fromLong(1L);
		final PK pk2 = PK.fromLong(2L);

		workerRole.deleteTask(pk1, runtimeConfigHolder);
		workerRole.deleteTask(pk2, runtimeConfigHolder);
		workerRole.deleteTasks(runtimeConfigHolder);


		verify(tasksQueueGateway).deleteTasks(pksArgumentCaptor.capture());
		assertThat(pksArgumentCaptor.getValue()).containsExactlyInAnyOrder(pk1, pk2);
	}


	@Test
	public void shouldForceDeleteWhenCallingBatchSizeIsReached()
	{
		final AuxiliaryTablesWorkerRole workerRole = getAuxiliaryTablesWorkerRole();

		final List<PK> pks = LongStream.generate(RandomUtils::nextLong)
		                               .limit(DELETE_BATCH_SIZE)
		                               .mapToObj(PK::fromLong)
		                               .collect(Collectors.toList());

		pks.forEach(pk -> workerRole.deleteTask(pk, runtimeConfigHolder));
		verify(tasksQueueGateway, never()).deleteTasks(any());

		final PK overBatchSizePk = PK.fromLong(1L);
		assertThat(overBatchSizePk).isNotNull();
		workerRole.deleteTask(overBatchSizePk, runtimeConfigHolder);

		verify(tasksQueueGateway).deleteTasks(pksArgumentCaptor.capture());
		final Set<PK> expectedPks = new ImmutableSet.Builder<PK>().addAll(pks).add(overBatchSizePk).build();
		assertThat(pksArgumentCaptor.getValue()).containsExactlyElementsOf(expectedPks);
	}

	@Test
	public void shouldForceDeleteWhenCallingDeleteWithForceFlag()
	{
		final AuxiliaryTablesWorkerRole workerRole = getAuxiliaryTablesWorkerRole();

		final List<PK> pks = LongStream.generate(RandomUtils::nextLong)
		                               .limit(DELETE_BATCH_SIZE / 2)
		                               .mapToObj(PK::fromLong)
		                               .collect(Collectors.toList());

		pks.forEach(pk -> workerRole.deleteTask(pk, runtimeConfigHolder));
		verify(tasksQueueGateway, never()).deleteTasks(any());

		final PK pkWithForceFlag = PK.fromLong(1L);
		assertThat(pkWithForceFlag).isNotNull();

		workerRole.deleteTask(pkWithForceFlag, true, runtimeConfigHolder);

		verify(tasksQueueGateway).deleteTasks(pksArgumentCaptor.capture());
		final Set<PK> expectedPks = new ImmutableSet.Builder<PK>().addAll(pks).add(pkWithForceFlag).build();
		assertThat(pksArgumentCaptor.getValue()).containsExactlyElementsOf(expectedPks);
	}

	@Test
	public void shouldTryToDeleteForcedTaskIfBatchDeleteFails()
	{
		final AuxiliaryTablesWorkerRole workerRole = getAuxiliaryTablesWorkerRole();

		final List<PK> pks = LongStream.generate(RandomUtils::nextLong)
		                               .limit(DELETE_BATCH_SIZE / 2)
		                               .mapToObj(PK::fromLong)
		                               .collect(Collectors.toList());

		doThrow(RuntimeException.class).when(tasksQueueGateway).deleteTasks(any());

		pks.forEach(pk -> workerRole.deleteTask(pk, runtimeConfigHolder));
		verify(tasksQueueGateway, never()).deleteTasks(any());

		final PK pkWithForceFlag = PK.fromLong(1L);
		assertThat(pkWithForceFlag).isNotNull();

		workerRole.deleteTask(pkWithForceFlag, true, runtimeConfigHolder);

		verify(tasksQueueGateway, times(2)).deleteTasks(pksArgumentCaptor.capture());

		final Set<PK> expectedFirstCallPks = new ImmutableSet.Builder<PK>().addAll(pks).add(pkWithForceFlag).build();
		assertThat(pksArgumentCaptor.getAllValues().get(0)).containsExactlyElementsOf(expectedFirstCallPks);
		assertThat(pksArgumentCaptor.getAllValues().get(1)).containsExactly(pkWithForceFlag);
	}

	@Test
	public void shouldTryToPreviousBatchOnNextTaskWhenFirstDeleteFailed()
	{
		final AuxiliaryTablesWorkerRole workerRole = getAuxiliaryTablesWorkerRole();

		final List<PK> pks = LongStream.generate(RandomUtils::nextLong)
		                               .limit(DELETE_BATCH_SIZE)
		                               .mapToObj(PK::fromLong)
		                               .collect(Collectors.toList());

		doThrow(RuntimeException.class).when(tasksQueueGateway).deleteTasks(any());

		pks.forEach(pk -> workerRole.deleteTask(pk, runtimeConfigHolder));
		verify(tasksQueueGateway, never()).deleteTasks(any());

		final PK overBatchSizePk1 = PK.fromLong(1L);
		final PK overBatchSizePk2 = PK.fromLong(2L);
		assertThat(overBatchSizePk1).isNotNull();
		assertThat(overBatchSizePk2).isNotNull();

		workerRole.deleteTask(overBatchSizePk1, runtimeConfigHolder);
		workerRole.deleteTask(overBatchSizePk2, runtimeConfigHolder);

		verify(tasksQueueGateway, times(2)).deleteTasks(pksArgumentCaptor.capture());

		final Set<PK> expectedPks1 = new ImmutableSet.Builder<PK>().addAll(pks).add(overBatchSizePk1).build();
		assertThat(pksArgumentCaptor.getAllValues().get(0)).containsExactlyElementsOf(expectedPks1);

		final Set<PK> expectedPks2 = new ImmutableSet.Builder<PK>().addAll(expectedPks1).add(overBatchSizePk2).build();
		assertThat(pksArgumentCaptor.getAllValues().get(1)).containsExactlyElementsOf(expectedPks2);
	}

	private AuxiliaryTablesWorkerRole getAuxiliaryTablesWorkerRole()
	{
		final AuxiliaryTablesWorkerRole workerRole = new AuxiliaryTablesWorkerRole();
		workerRole.setMetricRegistry(metricRegistry);
		workerRole.setGatewayFactory(gatewayFactory);
		return workerRole;
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
