/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl.gateways;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.task.impl.AuxiliaryTablesBasedTaskProvider;
import de.hybris.platform.task.impl.AuxiliaryTablesGatewayFactory;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Optional;
import java.util.TimeZone;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class SchedulerStateGatewayTest extends BaseGatewayTest
{

	@Resource
	private AuxiliaryTablesGatewayFactory auxiliaryTablesGatewayFactory;

	private SchedulerStateGateway gateway;

	private TimeZone defaultTimeZone;

	@Before
	public void setUp() throws Exception
	{
		disableTaskEngine();

		defaultTimeZone = TimeZone.getDefault();

		gateway = auxiliaryTablesGatewayFactory.getSchedulerStateGateway();

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

		if (defaultTimeZone != null)
		{
			TimeZone.setDefault(defaultTimeZone);
		}

		enableTaskEngine();
	}

	@Test
	public void insertSchedulerRow()
	{
		final boolean b = gateway.insertSchedulerRow(Instant.now(), AuxiliaryTablesBasedTaskProvider.VERSION);

		assertThat(b).isTrue();
	}

	@Test
	public void updateSchedulerRow()
	{
		final Instant oldTimeStamp = Instant.now().minusSeconds(20);
		final boolean b = gateway.insertSchedulerRow(oldTimeStamp, AuxiliaryTablesBasedTaskProvider.VERSION);
		assertThat(b).isTrue();
		final SchedulerState schedulerState = gateway.getSchedulerTimestamp().orElseThrow();

		final boolean b1 = gateway.updateSchedulerRow(Instant.now(), schedulerState.getLastActiveTs());
		assertThat(b1).isTrue();
	}

	@Test
	public void getSchedulerTimestamp() throws InterruptedException
	{
		final Instant oldTimeStamp = Instant.now().truncatedTo(ChronoUnit.MILLIS).minusSeconds(20);
		final boolean b = gateway.insertSchedulerRow(oldTimeStamp, AuxiliaryTablesBasedTaskProvider.VERSION);
		assertThat(b).isTrue();

		Thread.sleep(10);

		final Optional<SchedulerState> schedulerTimestamp = gateway.getSchedulerTimestamp();

		assertThat(schedulerTimestamp).isPresent();
		assertThat(schedulerTimestamp.get().getLastActiveTs().truncatedTo(ChronoUnit.MILLIS)).isLessThan(
				schedulerTimestamp.get().getDbNow());
	}

	@Test
	public void getSchedulerTimestampAfterInsertInDifferentTimeZoneWest() throws InterruptedException
	{
		final boolean b = gateway.insertSchedulerRow(null, AuxiliaryTablesBasedTaskProvider.VERSION);
		assertThat(b).isTrue();

		Thread.sleep(10);
		TimeZone.setDefault(TimeZone.getTimeZone("America/Chicago"));

		final Optional<SchedulerState> schedulerTimestamp = gateway.getSchedulerTimestamp();

		assertThat(schedulerTimestamp).isPresent();
		assertThat(schedulerTimestamp.get().getLastActiveTs().truncatedTo(ChronoUnit.MILLIS)).isLessThanOrEqualTo(
				schedulerTimestamp.get().getDbNow());
	}


	@Test
	public void getSchedulerTimestampAfterInsertInDifferentTimeZoneEast() throws InterruptedException
	{
		final boolean b = gateway.insertSchedulerRow(null, AuxiliaryTablesBasedTaskProvider.VERSION);
		assertThat(b).isTrue();

		Thread.sleep(10);
		TimeZone.setDefault(TimeZone.getTimeZone("Asia/Tokyo"));

		final Optional<SchedulerState> schedulerTimestamp = gateway.getSchedulerTimestamp();

		assertThat(schedulerTimestamp).isPresent();
		assertThat(schedulerTimestamp.get().getLastActiveTs().truncatedTo(ChronoUnit.MILLIS)).isLessThanOrEqualTo(
				schedulerTimestamp.get().getDbNow());
	}

	@Test
	public void getSchedulerTimestampAfterUpdateInDifferentTimeZoneWest() throws InterruptedException
	{
		final boolean b1 = gateway.insertSchedulerRow(null, AuxiliaryTablesBasedTaskProvider.VERSION);
		assertThat(b1).isTrue();

		Thread.sleep(10);
		final SchedulerState oldState = gateway.getSchedulerTimestamp().orElseThrow();
		assertThat(oldState.getLastActiveTs()).isLessThan(oldState.getDbNow());

		final boolean b2 = gateway.updateSchedulerRow(oldState.getDbNow(), oldState.getLastActiveTs());
		assertThat(b2).isTrue();

		TimeZone.setDefault(TimeZone.getTimeZone("America/Chicago"));
		Thread.sleep(10);
		final Optional<SchedulerState> newState = gateway.getSchedulerTimestamp();

		assertThat(newState).isPresent();
		assertThat(newState.get().getLastActiveTs()).isLessThan(newState.get().getDbNow());
	}

	@Test
	public void getSchedulerTimestampAfterUpdateInDifferentTimeZoneEast() throws InterruptedException
	{

		final boolean b1 = gateway.insertSchedulerRow(null, AuxiliaryTablesBasedTaskProvider.VERSION);
		assertThat(b1).isTrue();

		Thread.sleep(10);
		final SchedulerState oldState = gateway.getSchedulerTimestamp().orElseThrow();
		assertThat(oldState.getLastActiveTs()).isLessThan(oldState.getDbNow());

		final boolean b2 = gateway.updateSchedulerRow(oldState.getDbNow(), oldState.getLastActiveTs());
		assertThat(b2).isTrue();

		TimeZone.setDefault(TimeZone.getTimeZone("Asia/Tokyo"));
		Thread.sleep(10);

		final Optional<SchedulerState> newState = gateway.getSchedulerTimestamp();

		assertThat(newState).isPresent();
		assertThat(newState.get().getLastActiveTs()).isLessThan(newState.get().getDbNow());
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

}
