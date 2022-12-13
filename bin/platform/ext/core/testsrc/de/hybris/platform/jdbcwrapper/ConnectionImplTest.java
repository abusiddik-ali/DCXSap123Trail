/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.jdbcwrapper;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.UnitTest;

import java.sql.Connection;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@UnitTest
public class ConnectionImplTest
{
	@Mock
	private HybrisDataSource dataSource;
	@Mock
	private Connection conn;

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void shouldMarkConnectionAsFailedWhenExceptionThrownOnSettingAutoCommit() throws SQLException
	{

		doThrow(SQLException.class).when(conn).setAutoCommit(anyBoolean());

		final ConnectionImpl connection = new ConnectionImpl(dataSource, conn);

		assertThatThrownBy(() -> connection.setAutoCommit(true)).isInstanceOf(SQLException.class);
		assertThat(connection.gotError()).isTrue();
	}

	@Test
	public void shouldCallWrappedConnectionWhenCheckingIfClosedWithoutClosingBefore() throws SQLException
	{

		final ConnectionImpl connection = new ConnectionImpl(dataSource, conn);

		connection.isClosed();

		verify(conn).isClosed();

	}

	@Test
	public void shouldNotCallWrappedConnectionWhenCheckingIfClosedAfterClosing() throws SQLException
	{

		final ConnectionImpl connection = new ConnectionImpl(dataSource, conn);

		connection.close();
		connection.isClosed();

		verify(conn, never()).isClosed();

	}
}
