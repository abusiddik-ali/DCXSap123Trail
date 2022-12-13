/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.internal.converter.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.core.model.c2l.RegionModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import javax.annotation.Resource;

import org.assertj.core.api.ThrowableAssert;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;


@IntegrationTest
public class ItemModelConverterVerifyCollectionFieldTest extends ServicelayerBaseTest
{
	@Resource
	private ModelService modelService;

	private CountryModel country;
	private RegionModel region;

	@Before
	public void prepare()
	{
		country = modelService.create(CountryModel.class);
		region = modelService.create(RegionModel.class);

		region.setCountry(country);
		region.setIsocode("r1");
		country.setIsocode("c1");
	}

	@Test
	public void nullCollectionTest()
	{
		country.setRegions(null);

		modelService.saveAll(region, country);
	}

	@Test
	public void hashSetRegularTest()
	{
		country.setRegions(new HashSet(Collections.singletonList(region)));

		modelService.saveAll(region, country);
	}

	@Test
	public void hashSetWithNullValueTest()
	{
		country.setRegions(new HashSet(Arrays.asList(region, null)));

		final Throwable actual = ThrowableAssert.catchThrowable(() -> modelService.saveAll(region, country));

		assertThat(actual).isInstanceOf(ModelSavingException.class);
		assertThat(actual.getMessage()).contains("is a collection and contains at least one NULL value");
	}

	@Test
	public void arrayListTest()
	{
		country.setRegions(new ArrayList(Collections.singletonList(region)));

		modelService.saveAll(region, country);
	}

	@Test
	public void collectionsSinletonTest()
	{
		country.setRegions(Collections.singleton(region));

		modelService.saveAll(region, country);
	}

	@Test
	public void setOfTest()
	{
		country.setRegions(Set.of(region));

		modelService.saveAll(region, country);
	}

	@Test
	public void immutableSetOfTest()
	{
		country.setRegions(ImmutableSet.of());

		modelService.saveAll(region, country);
	}

	@Test
	public void immutableListOfTest()
	{
		country.setRegions(ImmutableList.of());

		modelService.saveAll(region, country);
	}

	@Test
	public void treeSetTest()
	{
		country.setRegions(new TreeSet());

		modelService.saveAll(region, country);
	}
}
