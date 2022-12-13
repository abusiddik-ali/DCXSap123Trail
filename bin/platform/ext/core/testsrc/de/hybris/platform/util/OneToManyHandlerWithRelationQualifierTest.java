/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.constants.CoreConstants;
import de.hybris.platform.jalo.order.AbstractOrderEntry;
import de.hybris.platform.jalo.type.CollectionType;
import de.hybris.platform.testframework.HybrisJUnit4Test;

import org.junit.Test;

@UnitTest
public class OneToManyHandlerWithRelationQualifierTest extends HybrisJUnit4Test
{
	OneToManyHandler<AbstractOrderEntry> handler = new OneToManyHandler<AbstractOrderEntry>(
			CoreConstants.TC.CARTENTRY,
			true,
			"order",
			"entryNumber",
			false,
			true,
			CollectionType.LIST
	).withRelationQualifier("entries");

	OneToManyHandler<AbstractOrderEntry> handlerWithNoExplicitRelationCode = new OneToManyHandler<>(
			CoreConstants.TC.CARTENTRY,
			true,
			"order",
			"entryNumber",
			false,
			true,
			CollectionType.LIST
	);

	@Test
	public void testHandlerRelationQualifierIsNull()
	{
		assertThat(handler.relationQualifier).isNotNull();
	}

	@Test
	public void testHandlerWithNoExplicitRelationCodeRelationQualifierIsNull()
	{
		assertThat(handlerWithNoExplicitRelationCode.relationQualifier).isNull();
	}

	@Test
	public void testRelationQualifiersAreEqual()
	{
		assertThat(handler.relationQualifier).isEqualTo(handler.relationsInfo.get().getRelationQualifier());
		assertThat(handler.relationQualifier).isEqualTo(handlerWithNoExplicitRelationCode.relationsInfo.get().getRelationQualifier());
	}
}
