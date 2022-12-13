/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt;

import de.hybris.platform.jalo.order.price.TaxInformation;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.TaxValueInfoCriteria;
import de.hybris.platform.util.TaxValue;

import javax.annotation.Resource;

import org.junit.Before;


public class DefaultFindTaxValuesServiceIntegrationTest extends AbstractFindTaxValuesStrategyTest
{
	@Resource
	FindPDTValueInfoStrategy<TaxValue, TaxInformation, TaxValueInfoCriteria> cachingFindTaxValueInfoStrategy;

	@Override
	public FindPDTValueInfoStrategy<TaxValue, TaxInformation, TaxValueInfoCriteria> findPDTValueInfoStrategy()
	{
		return cachingFindTaxValueInfoStrategy;
	}

	@Before
	public void enableEurope1CacheTaxes()
	{
		europe1CacheTaxesProperty.switchToValue("true");
	}


}
