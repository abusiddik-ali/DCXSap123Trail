/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.calculation;

public class CalculationServiceJaloCacheTest extends AbstractCalculationServiceTest
{
	@Override
	public void prepareSettings()
	{
		europe1CacheTaxesProperty.switchToValue("true");
		pdtCalculationModeProperty.switchToValue("jalo");
	}
}
