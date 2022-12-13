/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl.gateways;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.type.TypeService;
import de.hybris.platform.task.TaskModel;
import de.hybris.platform.task.impl.AuxiliaryTablesGatewayFactory;
import de.hybris.platform.task.impl.AuxiliaryTablesSchedulerRole;
import de.hybris.platform.task.impl.TasksProvider;
import de.hybris.platform.task.impl.gateways.WorkerStateGateway.WorkerRange;
import de.hybris.platform.task.impl.gateways.WorkerStateGateway.WorkerState;
import de.hybris.platform.util.MessageFormatUtils;

import java.sql.Date;
import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.BinaryOperator;

import javax.annotation.Resource;

import org.apache.commons.lang3.tuple.Pair;
import org.assertj.core.util.Lists;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.codahale.metrics.MetricRegistry;


@IntegrationTest
public class TasksQueueGatewayTest extends BaseGatewayTest
{

	@Resource
	private AuxiliaryTablesGatewayFactory auxiliaryTablesGatewayFactory;

	@Resource
	private MetricRegistry metricRegistry;

	@Resource
	private TypeService typeService;

	@Resource
	private ModelService modelService;

	private TestSchedulerRole schedulerRole;

	private TasksQueueGateway gateway;

	@Before
	public void setUp() throws Exception
	{
		disableTaskEngine();

		schedulerRole = new TestSchedulerRole(auxiliaryTablesGatewayFactory, metricRegistry, typeService);

		gateway = auxiliaryTablesGatewayFactory.getTasksQueueGateway();

		try
		{
			gateway.dropTable();
		}
		catch (final Exception ignore)
		{
		}
		gateway.createTable();

		assertTableExists(gateway.getTableName());
	}

	@After
	public void tearDown() throws Exception
	{
		try
		{
			gateway.dropTable();
		}
		catch (final Exception ignore)
		{
		}
		assertTableNotExists(gateway.getTableName());

		enableTaskEngine();
	}

	@Test
	public void shouldExecuteGetTasksForWorkerAndMarkForProcessingQuery()
	{

		final TaskModel task = modelService.create(TaskModel.class);
		task.setExecutionDate(Date.from(Instant.now().minus(1, ChronoUnit.HOURS)));
		task.setRunnerBean("notRequired");

		modelService.save(task);

		gateway.addTasks(schedulerRole.getTasksQuery(0, 1000), schedulerRole.getExpiredTasksQuery(0, 1000), Instant.now(), 0,
				1000);
		//TODO create some tasks, insert pks into tasks_queue table, run query and check the result

		final List<TasksProvider.VersionPK> pks = gateway.getTasksForWorkerAndMarkForProcessing(new WorkerRange(0, 1000), 20,
				new WorkerState(0,
						Duration.ofSeconds(
								20),
						false,
						Collections.emptySet()),
				Duration.ofSeconds(20));

		assertThat(pks).extracting(TasksProvider.VersionPK::getLongValue).contains(task.getPk().getLongValue());
	}

	@Test
	public void shouldExecuteGetTasksForWorkerAndMarkForProcessingQueryExclusiveMode()
	{
		final TaskModel task = modelService.create(TaskModel.class);
		task.setExecutionDate(Date.from(Instant.now().minus(1, ChronoUnit.HOURS)));
		task.setRunnerBean("notRequired");
		task.setNodeId(0);
		task.setNodeGroup("blah");

		modelService.save(task);

		gateway.addTasks(schedulerRole.getTasksQuery(0, 1000), schedulerRole.getExpiredTasksQuery(0, 1000), Instant.now(), 0,
				1000);
		//TODO create some tasks, insert pks into tasks_queue table, run query and check the result

		final List<TasksProvider.VersionPK> pks = gateway.getTasksForWorkerAndMarkForProcessing(new WorkerRange(0, 100), 20,
				new WorkerState(0,
						Duration.ofSeconds(
								20), true,
						Collections.singleton(
								"blah")),
				Duration.ofSeconds(20));

		assertThat(pks).extracting(TasksProvider.VersionPK::getLongValue).contains(task.getPk().getLongValue());
	}


	@Test
	public void shouldFillNullNodeIdAndNodeGroupWhenAddingTasksToQueue()
	{
		final TaskModel task = modelService.create(TaskModel.class);
		task.setExecutionDate(Date.from(Instant.now().minus(1, ChronoUnit.HOURS)));
		task.setRunnerBean("notRequired");
		task.setNodeId(null);
		task.setNodeGroup(null);

		modelService.save(task);

		gateway.addTasks(schedulerRole.getTasksQuery(0, 1000), schedulerRole.getExpiredTasksQuery(0, 1000), Instant.now(), 0,
				1000);


		final Pair<Long, String> r = jdbcTemplate.queryForObject(
				MessageFormatUtils.format("SELECT NODE_ID, NODE_GROUP FROM {0} WHERE PK=?", gateway.getTableName()),
				(resultSet, i) -> Pair.of(Long.valueOf(resultSet.getLong(1)), resultSet.getString(2)),
				task.getPk().getLongValue());

		assertThat(r.getLeft()).isEqualTo(-1);
		assertThat(r.getRight()).isEqualTo(gateway.getEmptyGroupValue());

		final List<TasksProvider.VersionPK> pks = gateway.getTasksForWorkerAndMarkForProcessing(new WorkerRange(0, 1000), 20,
				new WorkerState(0,
						Duration.ofSeconds(
								20),
						false,
						Collections.emptySet()),
				Duration.ofSeconds(20));

		assertThat(pks).extracting(TasksProvider.VersionPK::getLongValue).contains(task.getPk().getLongValue());
	}

	@Test
	public void shouldExecuteCleanQuery()
	{
		gateway.clean(Duration.ofMinutes(15));
	}

	@Test
	public void shouldExecuteCleanQueryWithLargeInterval()
	{
		gateway.clean(Duration.ofHours(5));
	}

	@Test
	public void shouldExecuteAddTasksQuery()
	{
		final TaskModel task = modelService.create(TaskModel.class);
		task.setExecutionDate(Date.from(Instant.now().minus(1, ChronoUnit.HOURS)));
		task.setRunnerBean("notRequired");

		modelService.save(task);

		final long count = gateway.addTasks(schedulerRole.getTasksQuery(0, 1000), schedulerRole.getExpiredTasksQuery(0, 1000),
				Instant.now(), 0, 1000);

		assertThat(count).isGreaterThan(0);

		final List<Long> pks = jdbcTemplate.queryForList(MessageFormatUtils.format("SELECT PK FROM {0}", gateway.getTableName()),
				Long.class);

		assertThat(pks).contains(task.getPk().getLongValue());
	}

	@Test
	public void shouldRandomlySetRangeWhenExecutingAddTasksQuery()
	{
		for (int i = 0; i < 20; i++)
		{
			final TaskModel task = modelService.create(TaskModel.class);
			task.setExecutionDate(Date.from(Instant.now().minus(1, ChronoUnit.HOURS)));
			task.setRunnerBean("notRequired");

			modelService.save(task);
		}

		final int rangeStart = 0;
		final int rangeEnd = 1000;

		final long count = gateway.addTasks(schedulerRole.getTasksQuery(rangeStart, rangeEnd), schedulerRole.getExpiredTasksQuery(
				rangeStart, rangeEnd),
				Instant.now(), rangeStart, rangeEnd);

		assertThat(count).isGreaterThan(0);

		final List<Long> ranges = jdbcTemplate.queryForList(
				MessageFormatUtils.format("SELECT RANGE_VALUE FROM {0} GROUP BY RANGE_VALUE", gateway.getTableName()),
				Long.class);

		assertThat(ranges).size().isGreaterThan(1);
		final Optional<Long> max = ranges.stream().reduce(BinaryOperator.maxBy(Long::compareTo));
		final Optional<Long> min = ranges.stream().reduce(BinaryOperator.minBy(Long::compareTo));
		assertThat(max).isPresent();
		assertThat(min).isPresent();
		assertThat(max.get()).isLessThanOrEqualTo(rangeEnd);
		assertThat(min.get()).isGreaterThanOrEqualTo(rangeStart);
	}

	@Test
	public void shouldExecuteGetTasksCountQuery()
	{
		gateway.getTasksCount();
	}

	@Test
	public void shouldExecuteGetConditionsToScheduleQuery()
	{
		gateway.getConditionsToSchedule(schedulerRole.getConditionsQuery(), Instant.now());
	}

	@Test
	public void shouldExecuteDeleteTasksQuery()
	{
		gateway.deleteTasks(Lists.newArrayList(PK.fromLong(123), PK.fromLong(234)));
	}

	@Test
	public void shouldExecuteUnlockTasksForWorkersQuery()
	{
		gateway.unlockTasksForWorkers(Lists.newArrayList(0, 1));
	}

	@Test
	public void shouldExecuteDropTable()
	{
		gateway.dropTable();
	}

	@Test
	public void shouldReturnTrueIfTableExists()
	{
		assertTableExists(gateway.getTableName());

		final boolean r = gateway.doesTableExist();

		assertThat(r).isTrue();
	}

	@Test
	public void shouldReturnFalseIfTableDoesNotExist()
	{
		gateway.dropTable();
		assertTableNotExists(gateway.getTableName());

		final boolean r = gateway.doesTableExist();

		assertThat(r).isFalse();
	}


	private static class TestSchedulerRole extends AuxiliaryTablesSchedulerRole
	{

		public TestSchedulerRole(final AuxiliaryTablesGatewayFactory gatewayFactory, final MetricRegistry metricRegistry,
		                         final TypeService typeService)
		{
			this.setGatewayFactory(gatewayFactory);
			this.setMetricRegistry(metricRegistry);
			this.setTypeService(typeService);
		}

		@Override
		protected String getConditionsQuery()
		{
			return super.getConditionsQuery();
		}

		@Override
		protected String getExpiredTasksQuery(final int rangeStart, final int rangeEnd)
		{
			return super.getExpiredTasksQuery(rangeStart, rangeEnd);
		}

		@Override
		protected String getTasksQuery(final int rangeStart, final int rangeEnd, final boolean processTriggerTask)
		{
			return super.getTasksQuery(rangeStart, rangeEnd, processTriggerTask);
		}

		@Override
		protected String getTasksQuery(final int rangeStart, final int rangeEnd)
		{
			return super.getTasksQuery(rangeStart, rangeEnd);
		}
	}
}
