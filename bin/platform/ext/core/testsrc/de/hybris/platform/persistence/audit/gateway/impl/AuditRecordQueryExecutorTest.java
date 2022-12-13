/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.audit.gateway.impl;

import static java.util.stream.Collectors.toList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.persistence.audit.gateway.AuditSqlQuery;
import de.hybris.platform.persistence.audit.gateway.JsonAuditRecord;

import java.util.List;
import java.util.stream.IntStream;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.PreparedStatementSetter;

@UnitTest
public class AuditRecordQueryExecutorTest
{
	private static final String QUERY_WITH_LIMIT = "SELECT FROM audit_table LIMIT ? OFFSET ?";
	private static final int SQL_LIMIT = 10;

	@Mock
	AuditRecordExtractor auditRecordExtractor;

	@Mock
	JdbcTemplate jdbcTemplate;


	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);

		final JsonAuditRecord jsonAuditRecord = mock(JsonAuditRecord.class);

		final List<JsonAuditRecord> auditRecordListChunk = IntStream.range(0, SQL_LIMIT)
		                                                            .mapToObj(i -> jsonAuditRecord)
		                                                            .collect(toList());

		when(jdbcTemplate.query(eq(QUERY_WITH_LIMIT), any(PreparedStatementSetter.class),
				any(AuditRecordExtractor.class))).thenReturn(auditRecordListChunk, auditRecordListChunk, auditRecordListChunk,
				List.of());

		when(auditRecordExtractor.getCountedRows()).thenReturn(SQL_LIMIT, SQL_LIMIT, SQL_LIMIT, 0);
	}

	@Test
	public void testExecuteQueryWithLimit()
	{
		final AuditRecordQueryExecutor executor = new AuditRecordQueryExecutor(auditRecordExtractor, jdbcTemplate,
				100);

		final AuditSqlQuery sqlQuery = spy(new AuditSqlQuery(QUERY_WITH_LIMIT, new Object[]{ SQL_LIMIT, 0 }, 0, 1));

		final List<JsonAuditRecord> auditRecordList = executor.executeQuery(sqlQuery);

		assertThat(auditRecordList).hasSize(30);

		verify(jdbcTemplate, times(4))
				.query(eq(QUERY_WITH_LIMIT), any(PreparedStatementSetter.class), any(AuditRecordExtractor.class));
		verify(sqlQuery, times(4)).incrementOffset();
	}
}