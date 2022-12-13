/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jdbcwrapper;

import org.apache.commons.pool2.impl.GenericObjectPoolConfig;

import java.sql.Connection;
import java.util.concurrent.atomic.AtomicBoolean;


/**
 * For testing error handling when
 * <ul>
 * <li>all connections fail</li>
 * <li>single connections fail</li>
 * <li>the pool reports connection errors in general</li>
 * </ul>
 *
 * @author hr, ag
 */
public class JUnitConnectionErrorCheckingJDBCConnectionPool extends ConnectionErrorCheckingJDBCConnectionPool
		implements JUnitJDBCConnectionPool
{
	private final JUnitConnectionStatus connectionStatus;
	private final JUnitJDBCConnectionFactory factory;
	private final AtomicBoolean suspendConnectionBorrowing = new AtomicBoolean(false);

	public JUnitConnectionErrorCheckingJDBCConnectionPool(final JUnitJDBCConnectionFactory factory,
	                                                      final GenericObjectPoolConfig cfg,
	                                                      final JUnitConnectionStatus connectionStatus)
	{
		super(factory, cfg, connectionStatus);
		this.connectionStatus = connectionStatus;
		this.factory = factory;
	}


	public boolean mustValidate(final Connection con)
	{
		return factory.mustValidate(con);
	}

	/**
	 * Allows manual activating / deActivating of connection error flag held by his pool.
	 *
	 * @param hasErrors
	 */
	public void setPoolHasConnectionErrors(final boolean hasErrors)
	{
		connectionStatus.setPoolHasConnectionErrors(hasErrors);
	}

	/**
	 * Allows manually marking a connection to fail validation upon next {@link #returnConnection(Connection)}.
	 *
	 * @param con
	 */
	@Override
	public void addFailingConnection(final Connection con)
	{
		factory.addFailingConnection(con);
	}

	/**
	 * Removes a connection from failing set.
	 *
	 * @param con
	 * @see #addFailingConnection(Connection)
	 */
	@Override
	public void removeFailingConnection(final Connection con)
	{
		factory.removeFailingConnection(con);
	}

	/**
	 * Allows to manually cause all connections to fail upon validate.
	 *
	 * @param allFail
	 */
	@Override
	public void setAllConnectionsFail(final boolean allFail)
	{
		factory.setAllConnectionsFail(allFail);
	}

	@Override
	public void resetTestMode()
	{
		factory.setAllConnectionsFail(false);
		connectionStatus.setPoolHasConnectionErrors(false);
		factory.removeAllFailingConnections();
	}

	/**
	 * Don't wait for testing. Otherwise we have hard times testing.
	 */
	@Override
	protected boolean mayTestAgain(final int intervalSeconds)
	{
		return true;
	}

	@Override
	public void resumeConnectionBorrowing()
	{
		suspendConnectionBorrowing.set(false);
	}

	@Override
	public void suspendConnectionBorrowing()
	{
		suspendConnectionBorrowing.set(true);
	}

	@Override
	public Connection borrowConnection() throws Exception
	{
		int counter = 0;
		while (suspendConnectionBorrowing.get() && counter < 25)
		{
			Thread.sleep(200);
			counter++;
		}
		return super.borrowConnection();
	}
}
