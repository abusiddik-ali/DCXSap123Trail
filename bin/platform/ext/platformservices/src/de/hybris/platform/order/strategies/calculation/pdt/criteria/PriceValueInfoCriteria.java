/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt.criteria;

import de.hybris.platform.core.model.product.UnitModel;
import de.hybris.platform.europe1.enums.ProductPriceGroup;
import de.hybris.platform.europe1.enums.UserPriceGroup;
import de.hybris.platform.order.exceptions.CalculationException;


public interface PriceValueInfoCriteria extends PDTCriteria
{
	ProductPriceGroup getProductGroup();

	UserPriceGroup getUserGroup();

	long getQuantity();

	UnitModel getUnit();

	boolean isGiveAwayMode();

	boolean isEntryRejected();

	@Override
	default void validate()
	{
		try
		{
			if (PDTCriteriaTarget.VALUE.equals(getPDTCriteriaTarget()))
			{
				validateForRows();
			}
			else if (PDTCriteriaTarget.INFORMATION.equals(getPDTCriteriaTarget()))
			{
				validateForInfo();
			}
		}
		catch (final CalculationException e)
		{
			throw new IllegalArgumentException(e);
		}

	}

	default void validateForRows() throws CalculationException
	{
		if (getProduct() == null && getProductGroup() == null)
		{
			throw new CalculationException("cannot match price without product and product group - at least one must be present");
		}
		if (getUser() == null && getUserGroup() == null)
		{
			throw new CalculationException("cannot match price without user and user group - at least one must be present");
		}
		if (getCurrency() == null)
		{
			throw new CalculationException("cannot match price without currency");
		}
		if (getDate() == null)
		{
			throw new CalculationException("cannot match price without date");
		}
		if (getUnit() == null)
		{
			throw new CalculationException("cannot match price without unit");
		}
	}

	default void validateForInfo() throws CalculationException
	{
		if (getProduct() == null && getProductGroup() == null)
		{
			throw new CalculationException(
					"cannot match price info without product and product group - at least one must be present");
		}
		if (getUser() == null && getUserGroup() == null)
		{
			throw new CalculationException("cannot match price info without user and user group - at least one must be present");
		}
		if (getCurrency() == null)
		{
			throw new CalculationException("cannot match price info without currency");
		}
		if (getDate() == null)
		{
			throw new CalculationException("cannot match price info without date");
		}
	}
}
