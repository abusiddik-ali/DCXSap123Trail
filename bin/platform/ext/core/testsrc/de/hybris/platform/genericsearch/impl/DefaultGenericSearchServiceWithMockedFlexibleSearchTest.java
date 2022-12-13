/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.genericsearch.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.GenericCondition;
import de.hybris.platform.core.GenericQuery;
import de.hybris.platform.core.Operator;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.genericsearch.GenericSearchService;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;


@IntegrationTest
public class DefaultGenericSearchServiceWithMockedFlexibleSearchTest extends ServicelayerTransactionalBaseTest
{
	@Resource
	private GenericSearchService genericSearchService;

	@Resource
	private FlexibleSearchService flexibleSearchService;

	private FlexibleSearchService mockedFlexibleSearchService;

	@Before
	public void setUp()
	{
		mockedFlexibleSearchService = Mockito.mock(FlexibleSearchService.class);
		((DefaultGenericSearchService) genericSearchService).setFlexibleSearchService(mockedFlexibleSearchService);
	}

	@After
	public void cleanUp()
	{
		((DefaultGenericSearchService) genericSearchService).setFlexibleSearchService(flexibleSearchService);
	}

	@Test
	public void testConversionToPolyglotQueryWhenPossible()
	{
		//given

		final GenericQuery query = new GenericQuery(CartModel._TYPECODE,
				GenericCondition.getComparison(CartModel.CODE, Operator.EQUAL, "code"));

		//when
		genericSearchService.search(query);

		//then
		final ArgumentCaptor<FlexibleSearchQuery> argumentCaptor = ArgumentCaptor.forClass(FlexibleSearchQuery.class);
		Mockito.verify(mockedFlexibleSearchService).search(argumentCaptor.capture());
		assertThat(argumentCaptor.getValue().getQuery()).startsWith("GET {Cart} WHERE {code}");
	}
}
