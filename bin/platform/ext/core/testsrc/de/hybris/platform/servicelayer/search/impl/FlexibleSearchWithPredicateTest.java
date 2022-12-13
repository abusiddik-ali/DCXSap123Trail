/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.search.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Predicate;

import javax.annotation.Resource;

import org.junit.Test;

@IntegrationTest
public class FlexibleSearchWithPredicateTest extends ServicelayerBaseTest
{
	private static final int TITLE_NUMBER = 10;
	private static final int TITLE_START = 4;
	private static final int TITLE_COUNT = 1;
	private static final int COLLECTOR_MAX_SIZE = 3;
	@Resource
	private FlexibleSearchService flexibleSearchService;
	@Resource
	private ModelService modelService;

	@Test
	public void testFSPredicateWithStart()
	{
		final List<TitleModel> titles = prepareTestTitles("TEST", TITLE_NUMBER);
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(
				"select {pk} from {Title} where {code} like 'TEST%' ORDER BY {code} asc");
		fQuery.setResultClassList(Collections.singletonList(TitleModel.class));

		fQuery.setStart(TITLE_START);

		final List<TitleModel> streamResult = new ArrayList<>();

		flexibleSearchService.processSearchRows(fQuery, createPredicate(streamResult));
		assertThat(streamResult).hasSize(3);
		assertThat(streamResult).containsExactlyElementsOf(titles.subList(TITLE_START, TITLE_START + COLLECTOR_MAX_SIZE));
	}

	private <T> Predicate<T> createPredicate(final List<T> streamResult)
	{
		return t -> {
			streamResult.add(t);
			return streamResult.size() < COLLECTOR_MAX_SIZE;
		};
	}

	@Test
	public void testFSPredicateWithLimitHigherThanCollectorSize()
	{
		final List<TitleModel> titles = prepareTestTitles("TEST", TITLE_NUMBER);
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(
				"select {pk} from {Title} where {code} like 'TEST%' ORDER BY {code} asc");
		fQuery.setResultClassList(Collections.singletonList(TitleModel.class));

		fQuery.setCount(TITLE_COUNT + COLLECTOR_MAX_SIZE);

		final List<TitleModel> streamResult = new ArrayList<>();
		flexibleSearchService.processSearchRows(fQuery, (createPredicate(streamResult)));
		assertThat(streamResult).hasSize(COLLECTOR_MAX_SIZE);
		assertThat(streamResult).containsExactlyElementsOf(titles.subList(0, COLLECTOR_MAX_SIZE));
	}

	@Test
	public void testFSPredicateWithLimit()
	{
		final List<TitleModel> titles = prepareTestTitles("TEST", TITLE_NUMBER);
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(
				"select {pk} from {Title} where {code} like 'TEST%' ORDER BY {code} asc");
		fQuery.setResultClassList(Collections.singletonList(TitleModel.class));

		fQuery.setCount(TITLE_COUNT);

		final List<TitleModel> streamResult = new ArrayList<>();
		flexibleSearchService.processSearchRows(fQuery, (createPredicate(streamResult)));
		assertThat(streamResult).hasSize(TITLE_COUNT);
		assertThat(streamResult).containsExactlyElementsOf(titles.subList(0, TITLE_COUNT));
	}

	@Test
	public void testFSPredicateWithStartAndLimit()
	{
		final List<TitleModel> titles = prepareTestTitles("TEST", TITLE_NUMBER);
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(
				"select {pk} from {Title} where {code} like 'TEST%' ORDER BY {code} asc");
		fQuery.setResultClassList(Collections.singletonList(PK.class));

		fQuery.setStart(TITLE_START);
		fQuery.setCount(TITLE_COUNT);

		final List<PK> streamResult = new ArrayList<>();
		flexibleSearchService.processSearchRows(fQuery, createPredicate(streamResult));
		assertThat(streamResult).hasSize(TITLE_COUNT);
		assertThat(streamResult.get(0)).isEqualTo(titles.get(TITLE_START).getPk());
	}

	private List<TitleModel> prepareTestTitles(final String prefix, final int number)
	{
		final List<TitleModel> titles = new ArrayList<>();
		for (int i = 0; i < number; i++)
		{
			final TitleModel title = modelService.create(TitleModel.class);
			title.setCode(String.format("%s%03d", prefix, i));
			titles.add(title);
		}
		modelService.saveAll(titles);
		titles.forEach(modelService::refresh);
		return titles;
	}
}
