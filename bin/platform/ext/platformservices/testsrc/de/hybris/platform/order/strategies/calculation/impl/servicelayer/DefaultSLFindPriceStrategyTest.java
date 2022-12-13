/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.impl.servicelayer;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.jalo.order.price.PriceInformation;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.order.strategies.calculation.FindPriceHook;
import de.hybris.platform.order.strategies.calculation.pdt.FindPDTValueInfoStrategy;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.PDTCriteriaFactory;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.PriceValueInfoCriteria;
import de.hybris.platform.util.PriceValue;

import java.util.Arrays;
import java.util.Collections;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;


@SuppressWarnings("javadoc")
@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultSLFindPriceStrategyTest
{
	private static final PriceValue DEFAULT_PRICE = new PriceValue("currencyIso", 10.9, true);
	private static final PriceValue CUSTOM_PRICE = new PriceValue("anotherCurrencyIso", 47.3, true);
	private static final PriceValue ANOTHER_CUSTOM_PRICE = new PriceValue("yetAnotherCurrencyIso", 3.3, true);
	@Mock
	private FindPDTValueInfoStrategy<PriceValue, PriceInformation, PriceValueInfoCriteria> findPriceValueInfoStrategy;
	@Mock
	private PDTCriteriaFactory pdtCriteriaFactory;
	@Mock
	private FindPriceHook findPriceHook;
	@Mock
	private FindPriceHook anotherFindPriceHook;
	@Mock
	private AbstractOrderEntryModel entry;

	@InjectMocks
	private DefaultSLFindPriceStrategy classUnderTest;

	@Before
	public void setup() throws CalculationException
	{
		when(findPriceValueInfoStrategy.getPDTValues(any())).thenReturn(Collections.singletonList(DEFAULT_PRICE));
		when(findPriceHook.isApplicable(entry)).thenReturn(true);
		when(findPriceHook.findCustomBasePrice(entry, DEFAULT_PRICE)).thenReturn(CUSTOM_PRICE);
		when(anotherFindPriceHook.isApplicable(entry)).thenReturn(true);
		when(anotherFindPriceHook.findCustomBasePrice(entry, DEFAULT_PRICE)).thenReturn(ANOTHER_CUSTOM_PRICE);
	}

	@Test
	public void testFindBasePriceDefault() throws CalculationException
	{
		final PriceValue result = classUnderTest.findBasePrice(entry);
		assertEquals(DEFAULT_PRICE, result);
	}

	@Test
	public void testFindBasePriceCustom() throws CalculationException
	{
		classUnderTest.setFindPriceHooks(Collections.singletonList(findPriceHook));
		final PriceValue result = classUnderTest.findBasePrice(entry);
		assertEquals(CUSTOM_PRICE, result);
	}

	@Test
	public void testFindBasePriceCustomNotApplicable() throws CalculationException
	{
		when(findPriceHook.isApplicable(entry)).thenReturn(false);
		classUnderTest.setFindPriceHooks(Collections.singletonList(findPriceHook));
		final PriceValue result = classUnderTest.findBasePrice(entry);
		assertEquals(DEFAULT_PRICE, result);
	}

	@Test
	public void testFindBasePriceCustomNull() throws CalculationException
	{
		when(findPriceHook.findCustomBasePrice(entry, DEFAULT_PRICE)).thenReturn(null);
		classUnderTest.setFindPriceHooks(Collections.singletonList(findPriceHook));
		final PriceValue result = classUnderTest.findBasePrice(entry);
		assertEquals(DEFAULT_PRICE, result);
	}

	@Test
	public void testFindBasePriceCustomTwoApplicable() throws CalculationException
	{
		classUnderTest.setFindPriceHooks(Arrays.asList(findPriceHook, anotherFindPriceHook));
		final PriceValue result = classUnderTest.findBasePrice(entry);
		assertEquals(CUSTOM_PRICE, result);
	}

	@Test
	public void testFindBasePriceCustomTwoApplicableReverseOrder() throws CalculationException
	{
		classUnderTest.setFindPriceHooks(Arrays.asList(anotherFindPriceHook, findPriceHook));
		final PriceValue result = classUnderTest.findBasePrice(entry);
		assertEquals(ANOTHER_CUSTOM_PRICE, result);
	}
}
