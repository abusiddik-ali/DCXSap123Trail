/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.impl.servicelayer;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.jalo.order.price.TaxInformation;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.order.strategies.calculation.FindTaxValuesStrategy;
import de.hybris.platform.order.strategies.calculation.pdt.FindPDTValueInfoStrategy;
import de.hybris.platform.product.BaseCriteria;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.PDTCriteriaFactory;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.TaxValueInfoCriteria;
import de.hybris.platform.util.TaxValue;

import java.util.Collection;
import java.util.List;

import org.springframework.beans.factory.annotation.Required;


/**
 * Default implementation of taxes resolver strategy {@link FindTaxValuesStrategy}).
 */
public class DefaultSLFindTaxValuesStrategy implements FindTaxValuesStrategy
{
	private FindPDTValueInfoStrategy<TaxValue, TaxInformation, TaxValueInfoCriteria> findTaxValueInfoStrategy;
	private PDTCriteriaFactory pdtCriteriaFactory;

	@Override
	public boolean isSLOnly()
	{
		return true;
	}

	@Override
	public Collection<TaxValue> findTaxValues(final AbstractOrderEntryModel entry) throws CalculationException
	{
		final TaxValueInfoCriteria taxCriteria = pdtCriteriaFactory.taxValueCriteriaFromOrderEntry(entry);
		return findTaxValueInfoStrategy.getPDTValues(taxCriteria);
	}

	@Override
	public List<TaxInformation> getTaxInformation(final BaseCriteria baseCriteria) throws CalculationException
	{
		return findTaxValueInfoStrategy.getPDTInformation(pdtCriteriaFactory.taxInfoCriteriaFromPriceCriteria(baseCriteria));
	}

	@Required
	public void setPdtCriteriaFactory(final PDTCriteriaFactory pdtCriteriaFactory)
	{
		this.pdtCriteriaFactory = pdtCriteriaFactory;
	}

	@Required
	public void setFindTaxValueInfoStrategy(
			final FindPDTValueInfoStrategy<TaxValue, TaxInformation, TaxValueInfoCriteria> findTaxValueInfoStrategy)
	{
		this.findTaxValueInfoStrategy = findTaxValueInfoStrategy;
	}
}
