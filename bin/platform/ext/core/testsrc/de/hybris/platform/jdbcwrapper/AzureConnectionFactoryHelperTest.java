/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.jdbcwrapper;

import static org.apache.commons.lang3.RandomStringUtils.randomAlphabetic;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.Config.DatabaseName;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.assertj.core.api.Assertions;
import org.junit.After;
import org.junit.Test;

import com.codahale.metrics.MetricRegistry;

@IntegrationTest
public class AzureConnectionFactoryHelperTest extends ServicelayerBaseTest
{

	private final PropertyConfigSwitcher dataSourcesToCheck = new PropertyConfigSwitcher(
			AzureConnectionFactoryHelper.PROPERTY_DATASOURCES_TO_CHECK_READ_ONLY);

	@After
	public void tearDown() throws Exception
	{
		dataSourcesToCheck.switchBackToDefault();
	}

	@Test
	public void shouldNotMarkConnectionAsReadOnlyWhenDataSourceNotInProperty() throws SQLException
	{
		dataSourcesToCheck.switchToValue("");

		final HybrisDataSource dataSource = createMockDataSource(DatabaseName.SQLSERVER);

		final AzureConnectionFactoryHelper helper = new AzureConnectionFactoryHelper(dataSource);

		final ConnectionImpl connection = createMockConnection();
		helper.applyAzureReadOnlySettings(connection);

		verify(connection, never()).setReadOnly(anyBoolean());
	}

	@Test
	public void shouldMarkConnectionAsReadOnlyWhenDataSourceNotInProperty() throws SQLException
	{
		final HybrisDataSource dataSource = createMockDataSource(DatabaseName.SQLSERVER);

		final String id = dataSource.getID();

		dataSourcesToCheck.switchToValue(
				String.join(",", randomAlphabetic(10), id, randomAlphabetic(10)));


		final AzureConnectionFactoryHelper helper = new AzureConnectionFactoryHelper(dataSource);
		final ConnectionImpl connection = createMockConnection(
				createMockResultSetWithResult(AzureConnectionFactoryHelper.AZURE_READ_ONLY_VALID_VALUE));

		helper.applyAzureReadOnlySettings(connection);
		verify(connection).setReadOnly(eq(true));
	}

	@Test
	public void shouldNotChangeTheReadOnlySettingIfCheckReturnedFalse() throws SQLException
	{

		final HybrisDataSource dataSource = createMockDataSource(DatabaseName.SQLSERVER);

		dataSourcesToCheck.switchToValue(dataSource.getID());

		final AzureConnectionFactoryHelper helper = new AzureConnectionFactoryHelper(dataSource);
		final ResultSet resultSet = createMockResultSetWithResult(randomAlphabetic(5));
		final ConnectionImpl connection = createMockConnection(resultSet);

		helper.applyAzureReadOnlySettings(connection);
		verify(resultSet).next();
		verify(connection, never()).setReadOnly(anyBoolean());

	}

	@Test
	public void shouldUserProvidedMetricRegistry()
	{

		final MetricRegistry metricRegistry = new MetricRegistry();
		final HybrisDataSource dataSource = createMockDataSource(DatabaseName.SQLSERVER);
		final AzureConnectionFactoryHelper helper = new AzureConnectionFactoryHelper(dataSource,
				metricRegistry);

		Assertions.assertThat(helper.getMetricRegistry()).isNotEmpty().hasValue(metricRegistry);
	}

	@Test
	public void shouldUserDefaultMetricRegistryIfNonSpecificInstanceHasBeenProvided()
	{
		final HybrisDataSource dataSource = createMockDataSource(DatabaseName.SQLSERVER);
		final AzureConnectionFactoryHelper helper = new AzureConnectionFactoryHelper(dataSource);

		Assertions.assertThat(helper.getMetricRegistry())
		          .isNotEmpty()
		          .hasValue(Registry.getApplicationContext().getBean("metricRegistry", MetricRegistry.class));
	}

	@Test
	public void shouldThrowExceptionWhenStatementFails() throws SQLException
	{

		final HybrisDataSource dataSource = createMockDataSource(DatabaseName.SQLSERVER);

		dataSourcesToCheck.switchToValue(dataSource.getID());

		final AzureConnectionFactoryHelper helper = new AzureConnectionFactoryHelper(dataSource);
		final ConnectionImpl connection = createMockConnectionThrowingException();

		assertThatThrownBy(() -> helper.applyAzureReadOnlySettings(connection)).isInstanceOf(SQLException.class);

	}

	@Test
	public void shouldCheckTheReadOnlyForSQLServerOnly()
	{
		for (final DatabaseName databaseName : DatabaseName.values())
		{
			final boolean expected = databaseName == DatabaseName.SQLSERVER;

			final HybrisDataSource mockDataSource = createMockDataSource(databaseName);
			dataSourcesToCheck.switchToValue(mockDataSource.getID());

			final AzureConnectionFactoryHelper helper = new AzureConnectionFactoryHelper(mockDataSource);
			assertThat(helper.shouldCheckAzureReadOnly()).as("Check the read-only verification is called for %s", databaseName.getName())
			                                             .isEqualTo(expected);
		}
	}

	private HybrisDataSource createMockDataSource(final DatabaseName databaseName)
	{
		final HybrisDataSource dataSource = mock(HybrisDataSource.class);
		final String datasourceId = randomAlphabetic(10);
		when(dataSource.getID()).thenReturn(datasourceId);
		when(dataSource.getDatabaseName()).thenReturn(databaseName.getName());
		when(dataSource.getTenant()).thenReturn(Registry.getCurrentTenant());
		return dataSource;
	}


	private ConnectionImpl createMockConnectionThrowingException() throws SQLException
	{
		final PreparedStatementImpl preparedStatement = mock(PreparedStatementImpl.class);
		when(preparedStatement.executeQuery()).thenThrow(new SQLException("mock exception"));
		return createMockConnection(preparedStatement);
	}

	private ConnectionImpl createMockConnection() throws SQLException
	{
		return createMockConnection((PreparedStatementImpl) null);
	}

	private ConnectionImpl createMockConnection(final ResultSet resultSet) throws SQLException
	{
		return createMockConnection(createMockPreparedStatement(resultSet));
	}

	private ConnectionImpl createMockConnection(final PreparedStatementImpl preparedStatement) throws SQLException
	{

		final ConnectionImpl connection = mock(ConnectionImpl.class);
		if (preparedStatement != null)
		{
			when(connection.prepareStatement(anyString())).thenReturn(preparedStatement);
		}
		return connection;
	}

	private PreparedStatementImpl createMockPreparedStatement(final ResultSet resultSet) throws SQLException
	{
		final PreparedStatementImpl preparedStatement = mock(PreparedStatementImpl.class);
		when(preparedStatement.executeQuery()).thenReturn(resultSet);
		return preparedStatement;
	}

	private ResultSet createMockResultSetWithResult(final String result) throws SQLException
	{
		final ResultSet resultSet = mock(ResultSet.class);
		when(resultSet.next()).thenReturn(true, false);
		when(resultSet.getString(anyInt())).thenReturn(result);
		return resultSet;
	}
}
