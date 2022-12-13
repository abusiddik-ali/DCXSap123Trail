/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt.processor;

import de.hybris.platform.jalo.order.price.PDTInformation;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.PDTCriteria;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.PDTCriteria.PDTCriteriaTarget;
import de.hybris.platform.util.PDTValue;

import java.util.List;


public class PDTContext<VALUE extends PDTValue, INFO extends PDTInformation, CRITERIA extends PDTCriteria>
{

	private CRITERIA criteria;
	private List<VALUE> values;
	private List<INFO> informations;

	public PDTContext(final CRITERIA criteria)
	{
		super();
		this.criteria = criteria;
	}

	public PDTCriteriaTarget getPDTCriteriaTarget()
	{
		return criteria.getPDTCriteriaTarget();
	}

	public CRITERIA getCriteria()
	{
		return criteria;
	}

	public void setCriteria(final CRITERIA request)
	{
		this.criteria = request;
	}

	public List<VALUE> getValues()
	{
		return values;
	}

	public void setValues(final List<VALUE> values)
	{
		this.values = values;
	}

	public List<INFO> getInformations()
	{
		return informations;
	}

	public void setInformations(final List<INFO> informations)
	{
		this.informations = informations;
	}

}
