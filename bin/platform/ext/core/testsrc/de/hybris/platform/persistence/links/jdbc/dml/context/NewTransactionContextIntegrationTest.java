/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.links.jdbc.dml.context;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.persistence.links.jdbc.dml.RelationModifictionContext;

import java.util.Date;


@IntegrationTest
public class NewTransactionContextIntegrationTest extends AbstractRelationModifictionContextIntegrationTest
{
	@Override
	protected RelationModifictionContext instantiateContext(final Date now)
	{
		return new NewTransactionContext(RELATION_CODE, writePersistenceGateway, true, now);
	}
}
