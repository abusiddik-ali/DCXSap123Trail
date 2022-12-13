/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jdbcwrapper;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.DataSourceImplFactory;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.Tenant;
import de.hybris.platform.core.threadregistry.RegistrableThread;
import de.hybris.platform.testframework.HybrisJUnit4Test;
import de.hybris.platform.util.Config;
import de.hybris.platform.util.Config.SystemSpecificParams;
import de.hybris.platform.util.config.ConfigIntf;

import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.apache.log4j.Logger;
import org.junit.Test;
import org.springframework.jdbc.support.JdbcUtils;

@IntegrationTest
public class OracleConnectionClosedTest extends HybrisJUnit4Test
{
	private static final Logger LOG = Logger.getLogger(OracleConnectionClosedTest.class);

	@Test
	public void shouldExecuteStatementOnPreviouslyInterruptedConnectionAfterReborrowingFromPool()
	{
		final Tenant currentTenant = Registry.getCurrentTenantNoFallback();

		final TestDataSourceImpl dataSource = new TestDataSourceImpl("testDataSource", currentTenant,
				createConnectionParams(currentTenant), false,
				new DataSourceImplFactory());

		final StatementRunner statementRunner = new StatementRunner(currentTenant, dataSource);
		final RegistrableThread thread = new RegistrableThread(statementRunner);

		thread.start();
		try
		{
			thread.join(5000);
		}
		catch (final InterruptedException e)
		{
			thread.interrupt();
			e.printStackTrace();
		}

		final ConnectionImpl connection;
		try
		{
			connection = (ConnectionImpl) dataSource.getConnection();

			assertThat(connection).isNotNull();

			simulateNormalConnection(connection, "third");

		}
		catch (final SQLException e)
		{
			fail("The connection should still be valid upon returning from pool", e);
		}
		finally
		{
			dataSource.destroy();
		}
	}


	private Map<String, String> createConnectionParams(final Tenant tenant)
	{
		final ConfigIntf cfg = tenant.getConfig();
		final Map<String, String> params = new HashMap<>(5);
		params.put(SystemSpecificParams.DB_USERNAME, cfg.getParameter(SystemSpecificParams.DB_USERNAME));
		params.put(SystemSpecificParams.DB_PASSWORD, cfg.getParameter(SystemSpecificParams.DB_PASSWORD));
		params.put(SystemSpecificParams.DB_URL, cfg.getParameter(SystemSpecificParams.DB_URL));
		params.put(SystemSpecificParams.DB_DRIVER, cfg.getParameter(SystemSpecificParams.DB_DRIVER));
		params.put(SystemSpecificParams.DB_TABLEPREFIX, cfg.getParameter(SystemSpecificParams.DB_TABLEPREFIX));
		params.put("db.customsessionsql", cfg.getParameter("db.customsessionsql"));

		return params;
	}


	private void simulateNormalConnection(final ConnectionImpl connection, final String statementSeriesName)
			throws SQLException
	{

		Statement stmt = null;
		try
		{
			OracleConnectionClosedTest.LOG.info("Creating statement: " + statementSeriesName);
			stmt = connection.createStatement();
			OracleConnectionClosedTest.LOG.info("Executing statement: " + statementSeriesName);
			stmt.executeQuery("SELECT COUNT(*) FROM " + Config.getString("db.tableprefix",
					"") + "ydeployments");
		}
		finally
		{
			OracleConnectionClosedTest.LOG.info("Closing statement: " + statementSeriesName);
			JdbcUtils.closeStatement(stmt);
		}
	}


	class StatementRunner implements Runnable
	{
		private final Tenant tenant;

		private final HybrisDataSource dataSource;

		StatementRunner(final Tenant tenant, final HybrisDataSource dataSource)
		{
			Objects.requireNonNull(tenant);
			Objects.requireNonNull(dataSource);

			this.tenant = tenant;
			this.dataSource = dataSource;
		}

		@Override
		public void run()
		{
			Registry.setCurrentTenant(tenant);

			try (final ConnectionImpl connection = (ConnectionImpl) dataSource.getConnection())
			{
				simulateNormalConnection(connection, "first");

				Thread.currentThread().interrupt();

				simulateNormalConnection(connection, "second");
			}
			catch (final SQLException e)
			{
				e.printStackTrace();
			}
			finally
			{
				Registry.unsetCurrentTenant();
			}
		}
	}


	class TestDataSourceImpl extends DataSourceImpl
	{
		TestDataSourceImpl(final String id, final Tenant tenant,
		                   final Map<String, String> connectionParams, final boolean readOnly,
		                   final DataSourceImplFactory factory)
		{
			super(tenant, id, connectionParams, readOnly, factory);
		}

		@Override
		protected GenericObjectPoolConfig createNewPoolConfig()
		{
			final GenericObjectPoolConfig newPoolConfig = super.createNewPoolConfig();

			newPoolConfig.setMaxTotal(1);
			newPoolConfig.setMaxIdle(1);
			newPoolConfig.setMinIdle(0);

			return newPoolConfig;
		}
	}
}
