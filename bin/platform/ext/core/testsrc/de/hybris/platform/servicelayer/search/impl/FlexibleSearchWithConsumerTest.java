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
import java.util.Locale;
import java.util.function.Consumer;

import javax.annotation.Resource;

import org.junit.Test;

@IntegrationTest
public class FlexibleSearchWithConsumerTest extends ServicelayerBaseTest
{
	@Resource
	private FlexibleSearchService flexibleSearchService;

	@Resource
	private ModelService modelService;

	private static final int TITLE_NUMBER = 10;
	private static final int TITLE_START = 4;
	private static final int TITLE_COUNT = 1;

	@Test
	public void testFSConsumerWithStart()
	{
		final List<TitleModel> titles = prepareTestTitles("TEST", TITLE_NUMBER);
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(
				"select {pk} from {Title} where {code} like 'TEST%' ORDER BY {code} asc");
		fQuery.setResultClassList(Collections.singletonList(TitleModel.class));

		fQuery.setStart(TITLE_START);

		final List<TitleModel> streamResult = new ArrayList<>();
		flexibleSearchService.processSearchRows(fQuery, ((Consumer<TitleModel>) streamResult::add));
		assertThat(streamResult).isNotEmpty();
		assertThat(streamResult.get(0)).isEqualTo(titles.get(TITLE_START));
	}

	@Test
	public void testFSConsumerWithLimit()
	{
		final List<TitleModel> titles = prepareTestTitles("TEST", TITLE_NUMBER);
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(
				"select {pk} from {Title} where {code} like 'TEST%' ORDER BY {code} asc");
		fQuery.setResultClassList(Collections.singletonList(TitleModel.class));

		fQuery.setCount(TITLE_COUNT);

		final List<TitleModel> streamResult = new ArrayList<>();
		flexibleSearchService.processSearchRows(fQuery, ((Consumer<TitleModel>) streamResult::add));
		assertThat(streamResult).hasSize(TITLE_COUNT);
		assertThat(streamResult.get(0)).isEqualTo(titles.get(0));
	}

	@Test
	public void testFSConsumerWithStartAndLimit()
	{
		final List<TitleModel> titles = prepareTestTitles("TEST", TITLE_NUMBER);
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(
				"select {pk} from {Title} where {code} like 'TEST%' ORDER BY {code} asc");
		fQuery.setResultClassList(Collections.singletonList(PK.class));

		fQuery.setStart(TITLE_START);
		fQuery.setCount(TITLE_COUNT);

		final List<Object> streamResult = new ArrayList<>();
		flexibleSearchService.processSearchRows(fQuery, ((Consumer<Object>) streamResult::add));
		assertThat(streamResult).hasSize(TITLE_COUNT);
		assertThat(streamResult.get(0)).isEqualTo(titles.get(TITLE_START).getPk());
	}

	@Test
	public void testFSConsumerWithStartAndLimitPolyglot()
	{
		final List<TitleModel> titles = prepareTestTitles("TEST", 1);
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(
				"get {Title} where {code}=?code");
		fQuery.addQueryParameter("code", "TEST000");
		fQuery.setResultClassList(Collections.singletonList(TitleModel.class));

		fQuery.setStart(0);
		fQuery.setCount(1);

		final List<TitleModel> streamResult = new ArrayList<>();
		flexibleSearchService.processSearchRows(fQuery, ((Consumer<TitleModel>) streamResult::add));
		assertThat(streamResult).hasSize(1);
		assertThat(streamResult.get(0)).isEqualTo(titles.get(0));
	}

	private List<TitleModel> prepareTestTitles(final String prefix, final int number)
	{
		final List<TitleModel> titles = new ArrayList<>();
		for (int i = 0; i < number; i++)
		{
			final TitleModel title = modelService.create(TitleModel.class);
			title.setCode(String.format(Locale.ROOT, "%s%03d", prefix, i));
			titles.add(title);
		}
		modelService.saveAll(titles);
		titles.forEach(modelService::refresh);
		return titles;
	}
}
