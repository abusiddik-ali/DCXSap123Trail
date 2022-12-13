/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.daos.impl;

import de.hybris.platform.core.model.order.payment.PaymentModeModel;
import de.hybris.platform.order.daos.PaymentModeDao;
import de.hybris.platform.servicelayer.internal.dao.DefaultGenericDao;
import de.hybris.platform.servicelayer.internal.dao.SortParameters;
import de.hybris.platform.servicelayer.internal.dao.SortParameters.SortOrder;

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class DefaultPaymentModeDao extends DefaultGenericDao<PaymentModeModel> implements PaymentModeDao
{
	public DefaultPaymentModeDao()
	{
		super(PaymentModeModel._TYPECODE);
	}

	private DefaultPaymentModeDao(final String typecode)
	{
		super(typecode);
	}

	@Override
	public List<PaymentModeModel> findPaymentModeForCode(final String code)
	{
		final Map<String, Object> params = new HashMap<String, Object>();
		params.put(PaymentModeModel.CODE, code);

		return find(params);
	}


	@Override
	public List<PaymentModeModel> findAllPaymentModes()
	{
		final SortParameters sortParameters = new SortParameters();
		sortParameters.addSortParameter(PaymentModeModel.CODE, SortOrder.ASCENDING);

		return find(sortParameters);
	}

}
