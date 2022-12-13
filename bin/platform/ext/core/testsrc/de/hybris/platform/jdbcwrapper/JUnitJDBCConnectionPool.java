/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jdbcwrapper;

import java.sql.Connection;


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
public interface JUnitJDBCConnectionPool
{
	void addFailingConnection(final Connection con);

	void removeFailingConnection(final Connection con);

	void setAllConnectionsFail(final boolean allFail);

	void resetTestMode();
	
	default void resumeConnectionBorrowing()
	{
		//API compatibility, do nothing
	}

	default void suspendConnectionBorrowing()
	{
		//API compatibility, do nothing
	}
}
