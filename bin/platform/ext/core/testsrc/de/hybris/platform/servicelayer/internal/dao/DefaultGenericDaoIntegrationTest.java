/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.internal.dao;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.jalo.flexiblesearch.QueryOptions;
import de.hybris.platform.persistence.flexiblesearch.polyglot.PolyglotPersistenceFlexibleSearchSupport;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.internal.dao.SortParameters.SortOrder;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.impl.SearchResultImpl;

import java.util.Collections;
import java.util.Map;

import org.junit.Test;
import org.mockito.ArgumentMatcher;
import org.mockito.Mockito;


@IntegrationTest
public class DefaultGenericDaoIntegrationTest extends ServicelayerBaseTest
{

	@Test
	public void shouldUsePolyglotDialectOnFind()
	{
		final DefaultGenericDao<?> dao = givenDao();

		dao.find();

		verifyPolyglotDialectWasUsed(dao);
	}

	@Test
	public void shouldUsePolyglotDialectOnFindWithFilterParameters()
	{
		final DefaultGenericDao<?> dao = givenDao();
		final Map<String, Object> filterParams = givenFilterParams();

		dao.find(filterParams);

		verifyPolyglotDialectWasUsed(dao);
	}

	@Test
	public void shouldUsePolyglotDialectOnFindWithSortParameters()
	{
		final DefaultGenericDao<?> dao = givenDao();
		final SortParameters sortParams = givenSortParams();

		dao.find(sortParams);

		verifyPolyglotDialectWasUsed(dao);
	}

	@Test
	public void shouldUsePolyglotDialectOnFindWithFilterAndSortParameters()
	{
		final DefaultGenericDao<?> dao = givenDao();
		final Map<String, Object> filterParams = givenFilterParams();
		final SortParameters sortParams = givenSortParams();

		dao.find(filterParams, sortParams);

		verifyPolyglotDialectWasUsed(dao);
	}

	@Test
	public void shouldUsePolyglotDialectOnFindWithFilterAndSortParametersAndCount()
	{
		final DefaultGenericDao<?> dao = givenDao();
		final Map<String, Object> filterParams = givenFilterParams();
		final SortParameters sortParams = givenSortParams();

		dao.find(filterParams, sortParams, 12);

		verifyPolyglotDialectWasUsed(dao);
	}

	private DefaultGenericDao<?> givenDao()
	{
		final FlexibleSearchService fsMock = Mockito.mock(FlexibleSearchService.class);
		when(fsMock.search(any(FlexibleSearchQuery.class)))
				.thenReturn(new SearchResultImpl<>(Collections.emptyList(), 0, 0, 0));

		final DefaultGenericDao dao = new DefaultGenericDao<>("Title");
		dao.setFlexibleSearchService(fsMock);
		return dao;
	}

	private Map<String, Object> givenFilterParams()
	{
		return Collections.singletonMap("code", "ala");
	}

	private SortParameters givenSortParams()
	{
		final SortParameters result = new SortParameters();
		result.addSortParameter("code", SortOrder.ASCENDING);
		return result;
	}

	private void verifyPolyglotDialectWasUsed(final DefaultGenericDao<?> dao)
	{
		final FlexibleSearchService fsMock = dao.getFlexibleSearchService();

		Mockito.verify(fsMock).search(Mockito.argThat(new IsValidPolyglotQuery()));
	}

	private static class IsValidPolyglotQuery extends ArgumentMatcher<FlexibleSearchQuery>
	{

		@Override
		public boolean matches(final Object argument)
		{
			if (!(argument instanceof FlexibleSearchQuery))
			{
				return false;
			}
			final FlexibleSearchQuery fsq = (FlexibleSearchQuery) argument;
			return PolyglotPersistenceFlexibleSearchSupport
					.tryToConvertToPolyglotCriteria(QueryOptions.newBuilder().withQuery(fsq.getQuery()).withValues(fsq.getQueryParameters()).build()).isPresent();
		}

	}

}
