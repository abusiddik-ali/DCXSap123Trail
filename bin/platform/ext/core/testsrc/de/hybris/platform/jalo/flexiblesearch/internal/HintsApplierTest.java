/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.jalo.flexiblesearch.internal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.jalo.flexiblesearch.hints.PreparedStatementHint;
import de.hybris.platform.jalo.flexiblesearch.hints.QueryHint;
import de.hybris.platform.jalo.flexiblesearch.hints.impl.JdbcHints;
import de.hybris.platform.jdbcwrapper.JdbcTestSupport;
import de.hybris.platform.persistence.property.JDBCValueMappings;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.util.Config;
import de.hybris.platform.util.Utilities;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Collections;
import java.util.Objects;
import java.util.UUID;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;


@IntegrationTest
public class HintsApplierTest extends ServicelayerBaseTest
{
	private static final String SELECT_QUERY = "SELECT {PK} FROM {User}";

	@Mock
	private PreparedStatement ps;

	private JdbcTestSupport.JdbcStatistics stats;

	@Before
	public void setUp()
	{
		MockitoAnnotations.initMocks(this);
		stats = JdbcTestSupport.createNewJdbcStatistics();
	}

	@After
	public void cleanUp()
	{
		stats.clear();
		stats.detach();
	}

	@Test
	public void shouldApplyFetchSizeHint() throws SQLException
	{
		// given
		final PreparedStatementHint hint = JdbcHints.preparedStatementHints()
																  .withFetchSize(50);

		// when
		HintsApplier.filterAndApplyPreparedStatementHints(ps, Collections.singletonList(hint));

		// then
		verify(ps).setFetchSize(50);
	}

	@Test
	public void shouldNotApplyFetchSizeForNullHint() throws SQLException
	{
		// when
		HintsApplier.filterAndApplyPreparedStatementHints(ps, Collections.singletonList(null));

		// then
		verify(ps, never()).setFetchSize(50);
	}

	@Test
	public void shouldApplyQueryHint()
	{
		// given
		final String hintText = UUID.randomUUID()
											 .toString();
		final QueryHint hint = TestHints.create(hintText);

		// when
		final FlexibleSearchQuery fq = new FlexibleSearchQuery(SELECT_QUERY);
		fq.addHints(hint);

		// then
		assertThat(HintsApplier.filterAndApplyQueryHints(fq.getQuery(), fq.getHints())).contains(SELECT_QUERY)
																												 .contains(hintText);
	}

	@Test
	public void shouldNotApplyQueryHintForNullHints()
	{
		// when
		final FlexibleSearchQuery fq = new FlexibleSearchQuery(SELECT_QUERY);

		// then
		assertThat(HintsApplier.filterAndApplyQueryHints(fq.getQuery(), null)).isEqualTo(SELECT_QUERY);
	}

	@Test
	public void shouldApplyQueryHintForTotalCountQuery() throws SQLException
	{
		// given
		stats.attachToCurrentThread();
		final String hintText = UUID.randomUUID()
											 .toString();
		final QueryHint hint = TestHints.create(hintText);
		Connection connection = null;

		// when
		try
		{
			connection = Registry.getCurrentTenant()
										.getDataSource()
										.getConnection();
			JDBCValueMappings.getInstance()
								  .getTotalCountFromCountQuery(connection,
										  "SELECT item_t0.PK FROM " + Config.getString("db.tableprefix", "")
												  + "titles item_t0 where item_t0.p_code = ? ",
										  Collections.singletonList("test"), Collections.singletonList(hint));
		}
		finally
		{
			Utilities.tryToCloseJDBC(connection, null, null, true);
		}

		// then
		stats.assertThat()
			  .selectStatements()
			  .hasOnlyOneElementSatisfying(s -> assertThat(s).contains(hintText));
	}

	private static class TestHints implements QueryHint
	{
		private final String text;

		TestHints(final String text)
		{
			this.text = text;
		}

		private static TestHints create(final String text)
		{
			return new TestHints(Objects.requireNonNull(text));
		}

		@Override
		public String apply(final String query)
		{
			return query + " -- " + text;
		}
	}
}
