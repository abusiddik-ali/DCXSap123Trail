/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.synchronization;

import de.hybris.platform.core.Registry;
import de.hybris.platform.jdbcwrapper.HybrisDataSource;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.Executors;
import java.util.concurrent.FutureTask;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

import org.apache.commons.lang3.time.DurationFormatUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.JdbcTemplate;

import com.google.common.base.Suppliers;


public class HsqldbSynchronizationFreezeLogger
{
	private static final Logger LOGGER = LoggerFactory.getLogger(HsqldbSynchronizationFreezeLogger.class);

	private static final Duration THREAD_DELAY = Duration.of(10, ChronoUnit.MINUTES);

	private static final Supplier<HsqldbSynchronizationFreezeLogger> INSTANCE = Suppliers
			.memoize(HsqldbSynchronizationFreezeLogger::new);

	private final ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();

	private final AtomicReference<FutureTask<Void>> currentLoggingTask = new AtomicReference<>();

	private HsqldbSynchronizationFreezeLogger()
	{
	}

	public void start()
	{

		final FutureTask<Void> task = new FutureTask<>(getLoggingRunnable());

		if (currentLoggingTask.compareAndSet(null, task))
		{
			LOGGER.info("starting logger thread in {}", DurationFormatUtils.formatDurationHMS(THREAD_DELAY.toMillis()));
			executorService.schedule(task, THREAD_DELAY.getSeconds(), TimeUnit.SECONDS);
		}
	}

	public void stop()
	{
		final FutureTask<Void> voidFutureTask = currentLoggingTask.get();
		if (voidFutureTask != null)
		{
			if (!voidFutureTask.isDone())
			{
				LOGGER.info("canceling logger thread");
				voidFutureTask.cancel(true);
			}
			else
			{
				LOGGER.info("logger thread has already finished");
			}

			currentLoggingTask.compareAndSet(voidFutureTask, null);
		}
	}

	private Callable<Void> getLoggingRunnable()
	{
		return () -> {

			try
			{
				LOGGER.info("calling logger task");


				final HybrisDataSource dataSource = Registry.getSlaveJunitTenant().getDataSource();

				final JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);

				final List<Map<String, Object>> maps = jdbcTemplate.queryForList(
						"SELECT * FROM INFORMATION_SCHEMA.SYSTEM_SESSIONS");

				LOGGER.info("rows {}", maps.size());
				for (final Map<String, Object> row : maps)
				{
					LOGGER.info("{}", row);
				}
			}
			catch (final Exception e)
			{
				LOGGER.error(e.getMessage(), e);
			}
			return null;
		};
	}


	public static void schedule()
	{
		INSTANCE.get().start();
	}


	public static void unschedule()
	{
		INSTANCE.get().stop();
	}
}
