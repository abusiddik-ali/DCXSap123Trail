/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.audit.gateway.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.persistence.audit.gateway.AuditSearchQuery;
import de.hybris.platform.persistence.audit.gateway.JsonAuditRecord;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.MessageFormat;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

@UnitTest
public class AuditRecordExtractorTest
{
	public static final String FIELD_A = "fieldA";
	public static final String FIELD_A_VALUE = "fieldA-value";
	public static final String FIELD_A_OTHER_VALUE = "fieldA-otherValue";
	public static final String FIELD_A_NEW_VALUE = "fieldA-newValue";
	public static final String FIELD_A_OTHER_NEW_VALUE = "fieldA-otherNewValue";
	public static final String FIELD_B = "fieldB";
	public static final String FIELD_B_VALUE = "fieldB-value";
	public static final String FIELD_B_OTHER_VALUE = "fieldB-otherValue";
	public static final String FIELD_B_NEW_VALUE = "fieldB-newValue";
	public static final String FIELD_B_OTHER_NEW_VALUE = "fieldB-otherNewValue";

	@Mock
	private ResultSet resultSet;

	@Mock
	private AuditRecordRowMapper auditRecordRowMapper;

	@Mock
	private JsonAuditRecord jsonAuditRecordA;

	@Before
	public void setUp() throws SQLException
	{
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void testExtractDataFromEmptyResultSet() throws SQLException
	{
		final AuditSearchQuery searchQuery = AuditSearchQuery.forType("someType")
		                                                     .withPayloadSearchRule(FIELD_A, FIELD_A_VALUE)
		                                                     .build();

		when(resultSet.next()).thenReturn(false);

		final AuditRecordExtractor extractor = new AuditRecordExtractor(searchQuery, auditRecordRowMapper);
		final List<JsonAuditRecord> jsonAuditRecords = extractor.extractData(resultSet);

		assertThat(jsonAuditRecords).isEmpty();
	}

	@Test
	public void testExtractDataWithPayloadSearchRuleWithEqualValue() throws SQLException
	{
		final AuditSearchQuery searchQuery = AuditSearchQuery.forType("someType")
		                                                     .withPayloadSearchRule(FIELD_A, FIELD_A_VALUE)
		                                                     .build();

		when(resultSet.getString("payloadbefore")).thenReturn(keyValuePair(FIELD_A, FIELD_A_VALUE),
				keyValuePair(FIELD_B, FIELD_B_VALUE));
		when(resultSet.getString("payloadafter")).thenReturn(keyValuePair(FIELD_A, FIELD_A_NEW_VALUE),
				keyValuePair(FIELD_B, FIELD_B_NEW_VALUE));

		when(auditRecordRowMapper.mapRow(eq(resultSet), anyInt())).thenReturn(jsonAuditRecordA);

		when(resultSet.next()).thenReturn(true, true, false);

		when(jsonAuditRecordA.getAttributeBeforeOperation(FIELD_A)).thenReturn(FIELD_A_VALUE);
		when(jsonAuditRecordA.getAttributeAfterOperation(FIELD_A)).thenReturn(FIELD_A_NEW_VALUE);

		final AuditRecordExtractor extractor = new AuditRecordExtractor(searchQuery, auditRecordRowMapper);
		final List<JsonAuditRecord> jsonAuditRecords = extractor.extractData(resultSet);

		assertThat(jsonAuditRecords).isNotEmpty().hasSize(1);
		assertThat(jsonAuditRecords.get(0)).isEqualTo(jsonAuditRecordA);
	}

	@Test
	public void testExtractDataWithPayloadSearchRuleWithNoneEqualValue() throws SQLException
	{
		final AuditSearchQuery searchQuery = AuditSearchQuery.forType("someType")
		                                                     .withPayloadSearchRule(FIELD_A, FIELD_A_VALUE)
		                                                     .build();

		when(resultSet.getString("payloadbefore")).thenReturn(keyValuePair(FIELD_A, FIELD_A_OTHER_VALUE),
				keyValuePair(FIELD_B, FIELD_B_OTHER_VALUE));
		when(resultSet.getString("payloadafter")).thenReturn(keyValuePair(FIELD_A, FIELD_A_NEW_VALUE),
				keyValuePair(FIELD_B, FIELD_B_OTHER_NEW_VALUE));

		when(auditRecordRowMapper.mapRow(eq(resultSet), anyInt())).thenReturn(jsonAuditRecordA);

		when(resultSet.next()).thenReturn(true, true, false);

		when(jsonAuditRecordA.getAttributeBeforeOperation(FIELD_A)).thenReturn(FIELD_A_OTHER_VALUE);
		when(jsonAuditRecordA.getAttributeAfterOperation(FIELD_A)).thenReturn(FIELD_A_OTHER_NEW_VALUE);

		final AuditRecordExtractor extractor = new AuditRecordExtractor(searchQuery, auditRecordRowMapper);
		final List<JsonAuditRecord> jsonAuditRecords = extractor.extractData(resultSet);

		assertThat(jsonAuditRecords).isEmpty();
	}


	@Test
	public void testGetCountedRows() throws SQLException
	{
		final int numberOfRows = 3;

		when(resultSet.next()).thenAnswer(new Answer<Boolean>()
		{
			private int counter = 0;

			@Override
			public Boolean answer(final InvocationOnMock invocation) throws Throwable
			{
				if (counter == numberOfRows)
				{
					return false;
				}
				counter++;

				return true;
			}
		});

		final AuditRecordExtractor extractor = new AuditRecordExtractor(AuditSearchQuery.forType("someType").build(),
				auditRecordRowMapper);
		extractor.extractData(resultSet);

		assertThat(extractor.getCountedRows()).isEqualTo(numberOfRows);
	}

	private String keyValuePair(final String... pairs)
	{
		final MessageFormat keyValueFormat = new MessageFormat("{0}=\"{1}\"");
		return keyValueFormat.format(pairs);
	}
}