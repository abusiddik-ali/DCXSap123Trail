/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateIfSingleResult;
import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

import de.hybris.platform.core.model.order.payment.PaymentModeModel;
import de.hybris.platform.order.PaymentModeService;
import de.hybris.platform.order.daos.PaymentModeDao;

import java.util.List;

import javax.annotation.Resource;

import org.springframework.beans.factory.annotation.Required;


/**
 *
 */
public class DefaultPaymentModeService implements PaymentModeService
{

	@Resource
	private PaymentModeDao paymentModeDao;

	@Required
	public void setPaymentModeDao(final PaymentModeDao paymentModeDao)
	{
		this.paymentModeDao = paymentModeDao;
	}


	@Override
	public PaymentModeModel getPaymentModeForCode(final String code)
	{
		validateParameterNotNullStandardMessage("code", code);

		final List<PaymentModeModel> res = paymentModeDao.findPaymentModeForCode(code);

		validateIfSingleResult(res, PaymentModeModel.class, "code", code);

		return res.get(0);
	}


	@Override
	public List<PaymentModeModel> getAllPaymentModes()
	{
		return paymentModeDao.findAllPaymentModes();
	}
}
