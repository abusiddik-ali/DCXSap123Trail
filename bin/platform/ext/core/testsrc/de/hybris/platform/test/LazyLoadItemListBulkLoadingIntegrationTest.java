/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test;


import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.LazyLoadItemList;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.WrapperFactory;
import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.jalo.c2l.Country;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.impl.LazyLoadModelList;
import de.hybris.platform.servicelayer.search.internal.resolver.ItemObjectResolver;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.persistence.PersistenceUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class LazyLoadItemListBulkLoadingIntegrationTest extends ServicelayerBaseTest
{
	@Resource
	private ModelService modelService;

	@Resource
	private ItemObjectResolver<Country> modelResolver;

	private final PropertyConfigSwitcher hjmpCacheSwitcher = new PropertyConfigSwitcher(
			WrapperFactory.LAZYLOADITEMLIST_CHECK_HJMP_CACHE);

	private static final int PAGE_SIZE = 2;

	@Before
	public void setUp()
	{
		Assume.assumeTrue(PersistenceUtils.isPersistenceLegacyModeEnabled());
	}

	@After
	public void cleanUp()
	{
		hjmpCacheSwitcher.switchBackToDefault();
	}

	@Test
	public void testBulkReadDirectlyWithUsingHjmpCache()
	{
		hjmpCacheSwitcher.switchToValue("true");
		final List<PK> pks = createCountries(4);

		final LazyLoadItemList<Country> itemList = new LazyLoadItemList<>(null, pks, PAGE_SIZE);
		final LazyLoadModelList<CountryModel> list = new LazyLoadModelList<>(itemList, PAGE_SIZE, Collections.emptyList(),
				modelResolver);

		list.forEach(e -> assertThat(e.getPk()).isNotNull());

		modifyObjectsInTransation(list.get(2), list.get(3));

		list.forEach(e -> assertThat(e.getPk()).isNotNull());

		assertThat(list.get(2).getName()).contains("test");
		assertThat(list.get(3).getName()).contains("test");
	}

	@Test
	public void testBulkReadDirectlyWithoutUsingHjmpCache()
	{
		hjmpCacheSwitcher.switchToValue("false");
		final List<PK> pks = createCountries(4);

		final LazyLoadItemList<Country> itemList = new LazyLoadItemList<>(null, pks, PAGE_SIZE);
		final LazyLoadModelList<CountryModel> list = new LazyLoadModelList<>(itemList, PAGE_SIZE, Collections.emptyList(),
				modelResolver);

		list.forEach(e -> assertThat(e.getPk()).isNotNull());

		modifyObjectsInTransation(list.get(2), list.get(3));

		list.forEach(e -> assertThat(e.getPk()).isNotNull());

		assertThat(list.get(2).getName()).contains("test");
		assertThat(list.get(3).getName()).contains("test");

	}

	private void modifyObjectsInTransation(final CountryModel model1, final CountryModel model2)
	{
		model1.setName("test-2");
		modelService.save(model1);

		model2.setName("test-3");
		modelService.save(model2);
	}

	private List<PK> createCountries(final int size)
	{
		final List<PK> pks = new ArrayList<>();
		for (int i = 0; i < size; i++)
		{
			final PK pk = createCountry("TEST1" + UUID.randomUUID()
																	.toString()).getPk();
			pks.add(pk);
		}

		return pks;
	}

	private CountryModel createCountry(final String isoCode)
	{
		final CountryModel myCountry = modelService.create(CountryModel._TYPECODE);
		myCountry.setIsocode(isoCode);
		modelService.save(myCountry);

		return myCountry;
	}
}
