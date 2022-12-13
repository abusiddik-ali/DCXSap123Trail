/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.flexiblesearch;


import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.Assume.assumeTrue;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.ServicelayerTransactionalTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.servicelayer.search.exceptions.FlexibleSearchException;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.Config;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.ImmutableMap;


@IntegrationTest
public class MSSQLInLimitationTest extends ServicelayerTransactionalTest
{
	private static final PropertyConfigSwitcher sqlServerInParamsLimit = new PropertyConfigSwitcher(
			"db.supported.inline.in.params.sqlserver");

	@Resource
	private ModelService modelService;
	@Resource
	private FlexibleSearchService flexibleSearchService;

	@Before
	public void setUp()
	{
		assumeTrue(Config.isSQLServerUsed());
	}

	@After
	public void tearDown()
	{
		sqlServerInParamsLimit.switchBackToDefault();
	}

	@Test
	public void shouldSelectOver2100Titles()
	{
		final int number = 2_200;
		final List<String> names = namesInRange(number);
		names.add(generateNameWitchChar('\''));

		createTitlesForNames(names);

		final SearchResult<Object> search = flexibleSearchService.search("SELECT {pk} FROM {Title} WHERE {name} in (?names)",
				ImmutableMap.of("names", names));

		final List<Object> result = search.getResult();
		assertThat(result).hasSize(names.size());
	}

	@Test
	public void shouldFailIfInlineParamsLimitDisabled() throws ClassNotFoundException
	{
		sqlServerInParamsLimit.switchToValue("0");
		final int number = 2_200;
		final List<String> names = namesInRange(number);
		names.add(generateNameWitchChar('\''));

		createTitlesForNames(names);
		assertThatThrownBy(() -> flexibleSearchService.search("SELECT {pk} FROM {Title} WHERE {name} in (?names)",
				ImmutableMap.of("names", names))).isInstanceOf(FlexibleSearchException.class)
		                                         .hasRootCauseInstanceOf(
				                                         (Class<? extends Throwable>) Class.forName(
						                                         "com.microsoft.sqlserver.jdbc.SQLServerException"));
	}

	@Test
	public void shouldNotFailWithOver2100TitlesWithParametersWithDifferentCharacters()
	{
		final int number = 2_200;
		final List<String> names = namesInRange(number);

		for (char x = 0; x <= 255; x++)
		{
			names.add(generateNameWitchChar(x));
		}
		names.add(generateNameWitchChar('\uDF27'));
		names.add(generateNameWitchChar('\u2727'));
		names.add(generateNameWitchChar('\u2700'));

		createTitlesForNames(names);

		final SearchResult<Object> search = flexibleSearchService.search("SELECT {pk} FROM {Title} WHERE {name} in (?names)",
				ImmutableMap.of("names", names));

		final List<Object> result = search.getResult();
		assertThat(result).isNotNull();
		assertThat(result.size()).isGreaterThan(number);
	}

	private String generateNameWitchChar(final Character c)
	{
		return "nam" + c + "e";
	}

	private List<String> namesInRange(final int to)
	{
		final List<String> names = new ArrayList<>();
		for (int i = 1; i <= to; ++i)
		{
			names.add("Title-" + i);
		}
		return names;
	}

	private void createTitlesForNames(final List<String> names)
	{
		for (int i = 0; i < names.size(); i++)
		{
			final TitleModel title = modelService.create(TitleModel.class);

			title.setCode("T-" + i);
			title.setName(names.get(i));
		}
		modelService.saveAll();
	}

	@Test
	public void shouldCorrectlyHandleJoinStatement()
	{
		final int number = 2_200;
		final List<String> names = createOrdersAndCorrespondingUsers(number);

		final List<Object> result = flexibleSearchService
				.search("SELECT {o.PK} FROM {Order AS o JOIN User AS u ON {o.user}={u.PK}} WHERE {u.name} IN (?names)",
						ImmutableMap.of("names", names))
				.getResult();

		assertThat(result).hasSize(number);
	}

	private List<String> createOrdersAndCorrespondingUsers(final int size)
	{
		final List<String> userNames = new ArrayList<>();

		final CurrencyModel curr = modelService.create(CurrencyModel.class);
		curr.setActive(Boolean.TRUE);
		curr.setIsocode("PLN");
		curr.setDigits(2);
		curr.setConversion(0.76d);
		curr.setSymbol("PLN");

		for (int i = 1; i <= size; ++i)
		{
			final UserModel testUser = modelService.create(UserModel.class);

			final String name = "testUser_" + i;
			testUser.setName(name);
			userNames.add(name);
			testUser.setUid("testUser_" + i);

			final OrderModel order = modelService.create(OrderModel.class);
			order.setDate(new Date());
			order.setCurrency(curr);
			order.setUser(testUser);
			order.setNet(Boolean.FALSE);
			order.setCode("test_order_" + i);
		}

		modelService.saveAll();
		return userNames;
	}
}
