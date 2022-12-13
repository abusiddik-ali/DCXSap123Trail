/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.audit.gateway;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;

import org.junit.Test;

@UnitTest
public class AuditSqlQueryTest
{

	@Test
	public void shouldIncrementOffsetWhenThereIsLimitAndOffsetParam()
	{
		// given
		final int limit = 100;
		final int initialOffset = 0;
		final int limitParamIndex = 0;
		final int offsetParamIndex = 1;
		final AuditSqlQuery auditSqlQuery = new AuditSqlQuery("Some query with limit ? and offset ?", new Object[]{limit, initialOffset},
				limitParamIndex, offsetParamIndex);
		assertThat(auditSqlQuery.getCurrentOffset()).isEqualTo(initialOffset);

		for(int incStep = 1; incStep <= 5 ; incStep++)
		{
			// when
			auditSqlQuery.incrementOffset();

			// then
			assertThat(auditSqlQuery.getCurrentOffset()).isEqualTo(incStep*limit);
			assertThat(auditSqlQuery.getQueryPrams()[offsetParamIndex]).isEqualTo(incStep*limit);
		}
	}

	@Test
	public void shouldNotIncrementOffsetWhenThereIsNoLimitOrOffsetParam()
	{
		// given
		final AuditSqlQuery auditSqlQuery = new AuditSqlQuery("Some query with limit ? and offset ?", new Object[]{}, -1,-1);
		assertThat(auditSqlQuery.getCurrentOffset()).isEqualTo(-1);

		for(int incStep = 1; incStep <= 5 ; incStep++)
		{
			// when
			auditSqlQuery.incrementOffset();

			// then
			assertThat(auditSqlQuery.getCurrentOffset()).isEqualTo(-1);
		}
	}

	@Test
	public void shouldBeQueryWithLimitWhenLimitParamIndexSet()
	{
		// given
		final int limit = 100;
		final int initialOffset = 0;
		final int limitParamIndex = 0;
		final int offsetParamIndex = 1;
		final AuditSqlQuery auditSqlQuery = new AuditSqlQuery("Some query with limit ? and offset ?", new Object[]{limit, initialOffset},
				limitParamIndex, offsetParamIndex);
		assertThat(auditSqlQuery.getCurrentOffset()).isEqualTo(initialOffset);

		// when
		// then
		assertThat(auditSqlQuery.isQueryWithLimit()).isTrue();
		assertThat(auditSqlQuery.getLimit()).isEqualTo(limit);
		assertThat(auditSqlQuery.getQueryPrams()[limitParamIndex]).isEqualTo(limit);
	}

	@Test
	public void shouldNotBeQueryWithLimitWhenLimitParamIndexNotSet()
	{
		// given
		final AuditSqlQuery auditSqlQuery = new AuditSqlQuery("Some query with limit ? and offset ?", new Object[]{}, -1,-1);
		assertThat(auditSqlQuery.getCurrentOffset()).isEqualTo(-1);

		// when
		// then
		assertThat(auditSqlQuery.isQueryWithLimit()).isFalse();
		assertThat(auditSqlQuery.getLimit()).isEqualTo(-1);
	}
}