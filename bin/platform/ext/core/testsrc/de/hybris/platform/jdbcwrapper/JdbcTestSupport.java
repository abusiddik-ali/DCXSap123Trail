/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jdbcwrapper;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.core.Registry;
import de.hybris.platform.jdbcwrapper.JDBCLogUtils.StatementsListener;

import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

import org.assertj.core.api.AbstractListAssert;
import org.assertj.core.api.AssertProvider;
import org.assertj.core.api.ObjectAssert;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;


public final class JdbcTestSupport
{
	private JdbcTestSupport()
	{
		//no instantiation
	}

	public static JdbcStatistics createNewJdbcStatistics()
	{
		return new JdbcStatistics();
	}

	public static class JdbcStatistics implements AssertProvider<JdbcStatisticsAssertions>
	{
		private final ConcurrentHashMap<Long, ImmutableList<String>> statementsPerThread = new ConcurrentHashMap<>();
		private final StatementsListener listener = JdbcStatistics.this::statementExecuted;

		private void statementExecuted(final String statement)
		{
			if (!JdbcStatisticsAssertions.isSupported(statement))
			{
				return;
			}
			statementsPerThread.computeIfPresent(getCurrentThreadId(),
					(tId, current) -> ImmutableList.<String>builder().addAll(current)
					                                                 .add(statement)
					                                                 .build());
		}

		public void attachToCurrentThread()
		{
			statementsPerThread.putIfAbsent(getCurrentThreadId(), ImmutableList.of());

			Registry.getCurrentTenant().getDataSource().getLogUtils().addListener(listener);
		}

		public void detach()
		{
			Registry.getCurrentTenant().getDataSource().getLogUtils().removeListener(listener);
		}

		public void clear()
		{
			statementsPerThread.computeIfPresent(getCurrentThreadId(), (tId, current) -> ImmutableList.of());
		}

		@Override
		public JdbcStatisticsAssertions assertThat()
		{
			return new JdbcStatisticsAssertions(
					ImmutableMap.<Long, ImmutableList<String>>builder().putAll(statementsPerThread).build());
		}

		private Long getCurrentThreadId()
		{
			return Thread.currentThread().getId();
		}
	}

	public static class JdbcStatisticsAssertions
	{
		private static final String SELECT = "SELECT";
		private static final Set<String> DMLS = ImmutableSet.of("INSERT", "UPDATE", "DELETE");
		private static final Set<String> ALL_SUPPORTED_STATEMENTS = ImmutableSet.<String>builder().addAll(DMLS).add(SELECT)
		                                                                                          .build();
		private final Map<Long, ImmutableList<String>> statementsPerThread;

		private JdbcStatisticsAssertions(final Map<Long, ImmutableList<String>> statements)
		{
			this.statementsPerThread = ImmutableMap.copyOf(statements);
		}

		private static boolean isSupported(final String statement)
		{
			return statement != null
					&& ALL_SUPPORTED_STATEMENTS.stream().anyMatch(s -> statement.toUpperCase(Locale.getDefault()).startsWith(s));
		}

		public void containsOnlySelectStatements()
		{
			assertThat(allStatements()).isNotEmpty().allMatch(JdbcStatisticsAssertions::isSELECT);
		}

		public void containsNoDMLStatements()
		{
			final Stream<String> dmlStatements = allStatements().filter(JdbcStatisticsAssertions::isDML);

			assertThat(dmlStatements).isEmpty();
		}

		public void containsNoStatements()
		{
			assertThat(allStatements()).isEmpty();
		}

		public void containsExpectedNumberOfStatements(final int expected)
		{
			assertThat(allStatements()).hasSize(expected);
		}

		private Stream<String> allStatements()
		{
			return statementsPerThread.values().stream().flatMap(s -> s.stream());
		}

		private static boolean isSELECT(final String statement)
		{
			return statement.toUpperCase(Locale.getDefault()).startsWith("SELECT");
		}

		private static boolean isDELETE(final String statement)
		{
			return statement.toUpperCase(Locale.getDefault()).startsWith("DELETE");
		}

		private static boolean isDML(final String statement)
		{
			return DMLS.stream().anyMatch(s -> statement.toUpperCase(Locale.getDefault()).startsWith(s));
		}

		public AbstractListAssert<?, List<? extends String>, String, ObjectAssert<String>> selectStatements()
		{
			return assertThat(allStatements()).filteredOn(JdbcStatisticsAssertions::isSELECT);
		}

		public AbstractListAssert<?, List<? extends String>, String, ObjectAssert<String>> deleteStatements()
		{
			return assertThat(allStatements()).filteredOn(JdbcStatisticsAssertions::isDELETE);
		}
	}
}
