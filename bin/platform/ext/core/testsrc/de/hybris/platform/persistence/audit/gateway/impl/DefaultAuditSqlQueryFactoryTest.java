/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.audit.gateway.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.persistence.audit.gateway.AuditSearchQuery;
import de.hybris.platform.persistence.audit.gateway.AuditSqlQuery;
import de.hybris.platform.persistence.audit.gateway.AuditSqlQueryFactory;
import de.hybris.platform.persistence.audit.gateway.AuditStorageUtils;
import de.hybris.platform.persistence.audit.gateway.GenericSearchRule;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.Config;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Assume;
import org.junit.Test;

@IntegrationTest
public class DefaultAuditSqlQueryFactoryTest extends ServicelayerBaseTest
{

	public static final String TYPE_CODE = "address";
	public static final String FIELD_NAME = "street";
	public static final String FIELD_VALUE = "some street";

	private final PropertyConfigSwitcher auditQueryLimitSap = new PropertyConfigSwitcher("audit.query.limit.sap");

	@Resource(name = "defaultAuditSqlQueryFactory")
	AuditSqlQueryFactory auditSqlQueryFactory;

	@After
	public void tearDown() throws Exception
	{
		auditQueryLimitSap.switchBackToDefault();
	}

	@Test
	public void testCreateSqlQueryForHanaDB()
	{
		Assume.assumeTrue(Config.isHanaUsed());

		final AuditSearchQuery auditSearchQuery = AuditSearchQuery.forType(TYPE_CODE).withSearchRule(new GenericSearchRule<>(
				FIELD_NAME,
				FIELD_VALUE, false)).build();

		final AuditSqlQuery sqlQuery = auditSqlQueryFactory.createSqlQuery(auditSearchQuery);

		assertThat(sqlQuery.getSqlQuery()).isEqualTo("SELECT * FROM " + AuditStorageUtils.getAuditTableName(
				TYPE_CODE) + " WHERE " + FIELD_NAME + "=? LIMIT ? OFFSET ?");

		final int dbSqlStatementLimit = Config.getInt("audit.query.limit." + Config.getDatabase(), -1);
		final int offset = 0;

		assertThat(sqlQuery.getQueryPrams()).isEqualTo(new Object[]{ FIELD_VALUE, dbSqlStatementLimit, offset });

	}

	@Test
	public void testCreateSqlQueryForDBThatDoesntRequireLimitAndOffsetPart()
	{
		Assume.assumeTrue(!Config.isHanaUsed());

		final AuditSearchQuery auditSearchQuery = AuditSearchQuery.forType(TYPE_CODE).withSearchRule(new GenericSearchRule<>(
				FIELD_NAME,
				FIELD_VALUE, false)).build();

		final AuditSqlQuery sqlQuery = auditSqlQueryFactory.createSqlQuery(auditSearchQuery);

		assertThat(sqlQuery.getSqlQuery()).isEqualTo("SELECT * FROM " + AuditStorageUtils.getAuditTableName(
				TYPE_CODE) + " WHERE " + FIELD_NAME + "=?");

		assertThat(sqlQuery.getQueryPrams()).isEqualTo(new Object[]{ FIELD_VALUE });
	}

	@Test
	public void testCreateStandardSqlQuery()
	{
		final AuditSearchQuery auditSearchQuery = AuditSearchQuery.forType(TYPE_CODE).withSearchRule(new GenericSearchRule<>(
				FIELD_NAME,
				FIELD_VALUE, false)).build();

		final AuditSqlQuery sqlQuery = auditSqlQueryFactory.createStandardSqlQuery(auditSearchQuery);

		assertThat(sqlQuery.getSqlQuery()).isEqualTo("SELECT * FROM " + AuditStorageUtils.getAuditTableName(
				TYPE_CODE) + " WHERE " + FIELD_NAME + "=?");

		assertThat(sqlQuery.getQueryPrams()).isEqualTo(new Object[]{ FIELD_VALUE });
	}

	@Test
	public void testCreateStandardSqlQueryEvenForDBWithQueryLimitWhenLimitInPropertiesSetToLessThanOne()
	{
		Assume.assumeTrue(Config.isHanaUsed());

		auditQueryLimitSap.switchToValue("0");

		final AuditSearchQuery auditSearchQuery = AuditSearchQuery.forType(TYPE_CODE).withSearchRule(new GenericSearchRule<>(
				FIELD_NAME,
				FIELD_VALUE, false)).build();

		final AuditSqlQuery sqlQuery = auditSqlQueryFactory.createSqlQuery(auditSearchQuery);

		assertThat(sqlQuery.getSqlQuery()).isEqualTo("SELECT * FROM " + AuditStorageUtils.getAuditTableName(
				TYPE_CODE) + " WHERE " + FIELD_NAME + "=?");

		assertThat(sqlQuery.getQueryPrams()).isEqualTo(new Object[]{ FIELD_VALUE });
	}
}