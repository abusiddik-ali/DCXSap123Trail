/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;

import de.hybris.platform.task.TaskService;
import de.hybris.platform.util.MessageFormatUtils;

import org.apache.commons.lang3.tuple.Pair;
import org.assertj.core.api.Assertions;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.BadSqlGrammarException;
import org.springframework.jdbc.core.JdbcTemplate;

public class AuxiliaryTablesTaskProviderTestHelper
{
	private final TaskService taskService;

	private final JdbcTemplate jdbcTemplate;

	private boolean taskEngineWasRunningBefore;

	public AuxiliaryTablesTaskProviderTestHelper(final TaskService taskService,
	                                             final JdbcTemplate jdbcTemplate)
	{
		this.taskService = taskService;
		this.jdbcTemplate = jdbcTemplate;
	}

	public void disableTaskEngine()
	{
		if (taskService.getEngine().isRunning())
		{
			taskEngineWasRunningBefore = true;
			taskService.getEngine().stop();
		}

		Assertions.assertThat(taskService.getEngine().isRunning()).isFalse();
	}

	public void enableTaskEngine()
	{
		if (taskEngineWasRunningBefore)
		{
			taskService.getEngine().start();
		}
	}

	public void assertTableExists(final String tableName)
	{
		final Pair<Integer, Integer> testInt = jdbcTemplate.queryForObject(
				MessageFormatUtils.format("SELECT 1, COUNT(*) FROM {0}", tableName),
				(resultSet, i) -> Pair.of(resultSet.getInt(1), resultSet.getInt(2)));

		Assertions.assertThat(testInt).isNotNull();
		Assertions.assertThat(testInt.getLeft()).isEqualTo(1);
	}

	public void assertTableNotExists(final String tableName)
	{
		LoggerFactory.getLogger(AuxiliaryTablesTaskProviderTestHelper.class)
		             .info("table {} should not be present in DB", tableName);
		Assertions.assertThatThrownBy(() -> jdbcTemplate.execute(MessageFormatUtils.format("SELECT * FROM {0}", tableName)))
		          .as("table %s should not exist", tableName)
		          .isInstanceOf(BadSqlGrammarException.class);
	}
}
