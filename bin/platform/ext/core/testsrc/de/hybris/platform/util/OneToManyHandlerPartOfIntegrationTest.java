/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util;

import static de.hybris.platform.util.OneToManyHandler.IGNORE_PART_OF_CONSTRAINT_FOR_TYPE_CODE;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.core.model.c2l.RegionModel;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionExecutionBody;
import de.hybris.platform.servicelayer.session.SessionService;

import java.util.List;
import java.util.Map;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class OneToManyHandlerPartOfIntegrationTest extends ServicelayerBaseTest
{
	@Resource
	SessionService sessionService;

	@Resource
	ModelService modelService;

	private CountryModel country1;
	private RegionModel region1;
	private RegionModel region2;

	@Before
	public void createInitialData()
	{
		modelService.detachAll();

		country1 = givenCountry("PARENT_1");
		region1 = givenRegion("CHILD_1");
		region2 = givenRegion("CHILD_2");

		region1.setCountry(country1);
		region2.setCountry(country1);

		modelService.saveAll();
	}

	@After
	public void removeInitialData()
	{
		modelService.removeAll(region1, region2, country1);
		modelService.detachAll();
	}

	@Test
	public void shouldFailWhenChangingParentForAPartOfItem()
	{
		var country2 = givenCountry("PARENT_2");
		country2.setRegions(List.of(region1));

		assertThatExceptionOfType(ModelSavingException.class)
				.isThrownBy(modelService::saveAll)
				.withRootCauseExactlyInstanceOf(PartOfItemAlreadyAssignedToTheParentException.class);
	}

	@Test
	public void shouldChangeParentForAPartOfItemWhenPartOfConstraintIsIgnored()
	{
		final CountryModel country2 = givenCountry("PARENT_2");
		country2.setRegions(List.of(region2));

		var sessionParams = Map.<String, Object>of(IGNORE_PART_OF_CONSTRAINT_FOR_TYPE_CODE, RegionModel._TYPECODE);
		sessionService.executeInLocalViewWithParams(sessionParams, inSession(modelService::saveAll));

		refresh(country1, country2, region1, region2);

		assertThat(country1.getRegions()).containsExactly(region1);
		assertThat(country2.getRegions()).containsExactly(region2);
		assertThat(region1.getCountry()).isSameAs(country1);
		assertThat(region2.getCountry()).isSameAs(country2);
	}

	@Test
	public void shouldChangeParentForAPartOfItemWhenPartOfConstraintIsIgnoredOnASuperType()
	{
		final CountryModel country2 = givenCountry("PARENT_2");
		country2.setRegions(List.of(region2));

		var sessionParams = Map.<String, Object>of(IGNORE_PART_OF_CONSTRAINT_FOR_TYPE_CODE, ItemModel._TYPECODE);
		sessionService.executeInLocalViewWithParams(sessionParams, inSession(modelService::saveAll));

		refresh(country1, country2, region1, region2);

		assertThat(country1.getRegions()).containsExactly(region1);
		assertThat(country2.getRegions()).containsExactly(region2);
		assertThat(region1.getCountry()).isSameAs(country1);
		assertThat(region2.getCountry()).isSameAs(country2);
	}

	@Test
	public void shouldFailWhenChangingParentForAPartOfItemWhenPartOfConstraintIsIgnoredForADifferentType()
	{
		final CountryModel country2 = givenCountry("PARENT_2");
		country2.setRegions(List.of(region2));

		var sessionParams = Map.<String, Object>of(IGNORE_PART_OF_CONSTRAINT_FOR_TYPE_CODE, TitleModel._TYPECODE);

		assertThatExceptionOfType(ModelSavingException.class)
				.isThrownBy(() ->
						sessionService.executeInLocalViewWithParams(sessionParams, inSession(modelService::saveAll)))
				.withRootCauseExactlyInstanceOf(PartOfItemAlreadyAssignedToTheParentException.class);
	}

	private SessionExecutionBody inSession(Runnable action)
	{
		return new SessionExecutionBody()
		{
			@Override
			public void executeWithoutResult()
			{
				action.run();
			}
		};
	}

	private CountryModel givenCountry(String isocode)
	{
		CountryModel country = modelService.create(CountryModel.class);

		country.setIsocode(isocode);

		return country;
	}

	private RegionModel givenRegion(String isocode)
	{
		RegionModel region = modelService.create(RegionModel.class);

		region.setIsocode(isocode);

		return region;
	}

	private void refresh(ItemModel... models)
	{
		for (var m : models)
		{
			modelService.refresh(m);
		}
	}
}