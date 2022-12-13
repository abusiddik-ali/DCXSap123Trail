/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt.postmatcher.impl;

import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.PDTCriteria;
import de.hybris.platform.order.strategies.calculation.pdt.postmatcher.PDTPostMatcher;

import java.util.Collection;


/**
 * This post matcher does nothing
 */
public class NoOpPDTPostMatcher implements PDTPostMatcher
{

	@Override
	public Collection process(final Collection models, final PDTCriteria criteria) throws CalculationException
	{
		return models;
	}

}
