/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.bootstrap.ddl.dbtypesystem.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;
import org.springframework.jdbc.core.JdbcTemplate;


@IntegrationTest
public class DefaultSQLRowsProviderTest extends ServicelayerBaseTest
{
	private RowsProvider rowsProvider;
	@Resource
	private JdbcTemplate jdbcTemplate;

	@Before
	public void setUp() throws Exception
	{
		rowsProvider = new DefaultSQLRowsProvider(jdbcTemplate, "junit_", "DEFAULT");
	}

	@Test
	public void testGetNumberSeriesRows() throws Exception
	{
		// when
		final Iterable<NumberSeriesRow> rows = rowsProvider.getNumberSeriesRows();

		// then
		assertThat(rows).isNotEmpty();
		for (final NumberSeriesRow row : rows)
		{
			assertThat(row.getSeriesKey()).startsWith("pk_");
			assertThat(row.getSeriesType()).isEqualTo(1);
		}
	}

	@Test
	public void testTypeRowMapper()
	{
		// when
		final Iterable<TypeRow> rows = rowsProvider.getTypeRows();

		// then no sql column not found exception should be thrown
		assertThat(rows).isNotEmpty();
	}
}
