/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt;

import de.hybris.platform.jalo.order.price.TaxInformation;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.TaxValueInfoCriteria;
import de.hybris.platform.util.TaxValue;

import org.junit.Before;


import javax.annotation.Resource;


public class GenericFindTaxValuesStrategyTest extends AbstractFindTaxValuesStrategyTest
{
	@Resource
	FindPDTValueInfoStrategy<TaxValue, TaxInformation, TaxValueInfoCriteria> findTaxValueInfoStrategy;


	@Override
	public FindPDTValueInfoStrategy<TaxValue, TaxInformation, TaxValueInfoCriteria> findPDTValueInfoStrategy()
	{
		return findTaxValueInfoStrategy;
	}

	@Before
	public void disableEurope1CacheTaxes() throws Exception
	{
		createCoreData();
		europe1CacheTaxesProperty.switchToValue("false");
	}


}
