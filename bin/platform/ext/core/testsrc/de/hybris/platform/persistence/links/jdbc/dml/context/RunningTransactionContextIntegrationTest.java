/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.links.jdbc.dml.context;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.jalo.ConsistencyCheckException;
import de.hybris.platform.persistence.links.jdbc.dml.RelationModifictionContext;
import de.hybris.platform.tx.Transaction;

import java.util.Date;

import org.junit.After;
import org.junit.Before;


@IntegrationTest
public class RunningTransactionContextIntegrationTest extends AbstractRelationModifictionContextIntegrationTest
{
	@Override
	@Before
	public void setUp() throws ConsistencyCheckException
	{
		Transaction.current().begin();
		super.setUp();
	}

	@After
	public void tearDown()
	{
		Transaction.current().rollback();
	}

	@Override
	protected RelationModifictionContext instantiateContext(final Date now)
	{
		return new RunningTransactionContext(RELATION_CODE, writePersistenceGateway, true, now);
	}
}
