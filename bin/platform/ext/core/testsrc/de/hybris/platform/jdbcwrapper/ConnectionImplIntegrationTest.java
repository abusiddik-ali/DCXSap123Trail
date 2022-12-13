/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.jdbcwrapper;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.DataSourceFactory;
import de.hybris.platform.core.DataSourceImplFactory;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.threadregistry.RegistrableThread;
import de.hybris.platform.jdbcwrapper.interceptor.JDBCInterceptor;
import de.hybris.platform.jdbcwrapper.interceptor.JDBCInterceptorContext;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.util.Config.SystemSpecificParams;
import de.hybris.platform.util.Utilities;
import de.hybris.platform.util.config.ConfigIntf;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.commons.lang3.RandomStringUtils;
import org.junit.After;
import org.junit.Test;

@IntegrationTest
public class ConnectionImplIntegrationTest extends ServicelayerBaseTest
{

	private DataSourceImpl dataSource;

	@After
	public void tearDown() throws Exception
	{
		if (dataSource != null)
		{
			dataSource.destroy();
		}
	}

	@Test
	public void shouldEnableConnectionPoolVerificationModeWhenExceptionInConnectionIsThrownButBeforeReturningToPool()
			throws InterruptedException
	{
		final AtomicBoolean shouldThrowError = new AtomicBoolean(false);

		final JDBCInterceptor errorThrowingJDBCInterceptor = errorThrowingJDBCInterceptor(shouldThrowError);
		final DataSourceFactory dataSourceFactory = dataSourceFactory(errorThrowingJDBCInterceptor);
		dataSource = dataSource(dataSourceFactory);

		final CountDownLatch waitToVerify = new CountDownLatch(1);
		final CountDownLatch waitToFinish = new CountDownLatch(1);

		final JDBCConnectionPool connectionPool = dataSourceFactory.createConnectionPool(dataSource,
				dataSource.createNewPoolConfig());

		assertThat(connectionPool).isInstanceOf(ConnectionErrorCheckingJDBCConnectionPool.class);

		assertThat(connectionPool.getNumActive()).isZero();

		final ExecutorService executorService = Executors.newSingleThreadExecutor(RegistrableThread::new);
		final Future<Long> future = executorService.submit(
				new SimpleJdbcLogic(connectionPool, waitToVerify,
						() -> shouldThrowError.set(true),
						() -> waitToFinish.await(10, TimeUnit.SECONDS)));

		//wait for the exception to be thrown in the thread
		waitToVerify.await(10, TimeUnit.SECONDS);

		//verify the verification mode is enabled in connection pool before returning the connection to pool
		assertThat(connectionPool).extracting("connectionStatus").hasOnlyElementsOfType(ConnectionStatus.class)
		                          .extracting(o -> ((ConnectionStatus) o).hadError()).containsOnly(true);

		assertThat(connectionPool.getNumActive()).isEqualTo(1);

		waitToFinish.await(10, TimeUnit.SECONDS);
		assertThatThrownBy(() -> future.get(5, TimeUnit.SECONDS)).hasCauseInstanceOf(SQLException.class);
	}

	private DataSourceImpl dataSource(final DataSourceFactory dataSourceFactory)
	{
		final ConfigIntf cfg = Registry.getCurrentTenantNoFallback().getConfig();
		final Map<String, String> params = new HashMap<>(5);
		params.put(SystemSpecificParams.DB_USERNAME, cfg.getParameter(SystemSpecificParams.DB_USERNAME));
		params.put(SystemSpecificParams.DB_PASSWORD, cfg.getParameter(SystemSpecificParams.DB_PASSWORD));
		params.put(SystemSpecificParams.DB_URL, cfg.getParameter(SystemSpecificParams.DB_URL));
		params.put(SystemSpecificParams.DB_DRIVER, cfg.getParameter(SystemSpecificParams.DB_DRIVER));
		params.put(SystemSpecificParams.DB_TABLEPREFIX, cfg.getParameter(SystemSpecificParams.DB_TABLEPREFIX));

		return (DataSourceImpl) dataSourceFactory.createDataSource(
				RandomStringUtils.randomAlphabetic(10),
				Registry.getCurrentTenantNoFallback(), params, false);
	}

	protected DataSourceFactory dataSourceFactory(final JDBCInterceptor jdbcInterceptor)
	{
		return new DataSourceImplFactory()
		{
			@Override
			public Connection wrapConnection(final HybrisDataSource wrappedDataSource, final Connection rawConnection)
			{
				return super.wrapConnection(wrappedDataSource, jdbcInterceptor.wrap(rawConnection, Connection.class));
			}

			@Override
			protected boolean isDatabaseConnectionErrorDetectionEnabled(final HybrisDataSource dataSource)
			{
				return true;
			}
		};
	}

	protected JDBCInterceptor errorThrowingJDBCInterceptor(final AtomicBoolean shouldThrowError)
	{
		return new JDBCInterceptor()
		{
			@Override
			public <T> T get(final JDBCInterceptorContext ctx, final SupplierWithSQLException<T> supplier)
					throws SQLException
			{
				if (shouldThrowError.get())
				{
					throw new SQLException("you shall not pass!");
				}
				return supplier.get();
			}
		};
	}

	@FunctionalInterface
	private interface Callback
	{
		void execute() throws Exception;
	}

	private static class SimpleJdbcLogic implements Callable<Long>
	{
		private final JDBCConnectionPool connectionPool;
		private final CountDownLatch markLogicFinished;

		private final Callback afterConnectionBorrowed;
		private final Callback beforeConnectionClosed;

		public SimpleJdbcLogic(final JDBCConnectionPool connectionPool,
		                       final CountDownLatch markLogicFinished,
		                       final Callback afterConnectionBorrowed, final Callback beforeConnectionClosed)
		{
			this.connectionPool = connectionPool;
			this.markLogicFinished = markLogicFinished;
			this.afterConnectionBorrowed = afterConnectionBorrowed;
			this.beforeConnectionClosed = beforeConnectionClosed;
		}

		@Override
		public Long call() throws Exception
		{

			Connection connection = null;
			ResultSet resultSet = null;
			PreparedStatement stmt = null;
			try
			{
				connection = connectionPool.borrowConnection();

				afterConnectionBorrowed.execute();

				stmt = connection.prepareStatement("SELECT COUNT(*) FROM COMPOSEDTYPES");

				resultSet = stmt.executeQuery();
				resultSet.next();
				return resultSet.getLong(1);
			}
			finally
			{
				markLogicFinished.countDown();
				beforeConnectionClosed.execute();
				Utilities.tryToCloseJDBC(connection, stmt, resultSet, true);
			}
		}
	}
}
