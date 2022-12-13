/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.impl.servicelayer;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.jalo.order.price.PriceInformation;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.order.strategies.calculation.FindPriceHook;
import de.hybris.platform.order.strategies.calculation.FindPriceStrategy;
import de.hybris.platform.order.strategies.calculation.pdt.FindPDTValueInfoStrategy;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.PDTCriteriaFactory;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.PriceValueInfoCriteria;
import de.hybris.platform.product.BaseCriteria;
import de.hybris.platform.util.PriceValue;

import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.annotation.Required;


/**
 * Default implementation of price resolver strategy ({@link FindPriceStrategy}, that resolves values for calculation.
 */
public class DefaultSLFindPriceStrategy implements FindPriceStrategy
{
	private FindPDTValueInfoStrategy<PriceValue, PriceInformation, PriceValueInfoCriteria> findPriceValueInfoStrategy;
	private PDTCriteriaFactory pdtCriteriaFactory;
	private List<FindPriceHook> findPriceHooks = Collections.emptyList();

	@Override
	public boolean isSLOnly()
	{
		return true;
	}

	@Override
	public PriceValue findBasePrice(final AbstractOrderEntryModel entry) throws CalculationException
	{
		final PriceValue defaultPrice = findBasePriceWithStrategy(entry);
		return getFindPriceHooks().stream().filter(h -> h.isApplicable(entry)).findFirst()
				.map(h -> h.findCustomBasePrice(entry, defaultPrice)).orElse(defaultPrice);
	}

	private PriceValue findBasePriceWithStrategy(final AbstractOrderEntryModel entry) throws CalculationException
	{
		final PriceValueInfoCriteria priceValuesCriteria = pdtCriteriaFactory.priceValueCriteriaFromOrderEntry(entry);

		final List<PriceValue> pdtValues = findPriceValueInfoStrategy.getPDTValues(priceValuesCriteria);

		return pdtValues.isEmpty() ? null : pdtValues.get(0);
	}

	@Override
	public List<PriceInformation> getPriceInformation(final BaseCriteria priceCriteria) throws CalculationException
	{
		final PriceValueInfoCriteria pvc = pdtCriteriaFactory.priceInfoCriteriaFromBaseCriteria(priceCriteria);
		return findPriceValueInfoStrategy.getPDTInformation(pvc);
	}

	@Required
	public void setPdtCriteriaFactory(final PDTCriteriaFactory pdtCriteriaFactory)
	{
		this.pdtCriteriaFactory = pdtCriteriaFactory;
	}

	@Required
	public void setFindPriceValueInfoStrategy(
			final FindPDTValueInfoStrategy<PriceValue, PriceInformation, PriceValueInfoCriteria> findPriceValueInfoStrategy)
	{
		this.findPriceValueInfoStrategy = findPriceValueInfoStrategy;
	}

	public void setFindPriceHooks(final List<FindPriceHook> findPriceHooks)
	{
		this.findPriceHooks = findPriceHooks;
	}

	protected List<FindPriceHook> getFindPriceHooks()
	{
		return findPriceHooks;
	}


	protected PDTCriteriaFactory getPdtCriteriaFactory()
	{
		return pdtCriteriaFactory;
	}
}
