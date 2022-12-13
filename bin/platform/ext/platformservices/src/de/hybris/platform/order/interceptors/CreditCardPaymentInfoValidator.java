/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.interceptors;

import de.hybris.platform.core.model.order.payment.CreditCardPaymentInfoModel;
import de.hybris.platform.order.strategies.paymentinfo.CreditCardNumberHelper;
import de.hybris.platform.servicelayer.exceptions.BusinessException;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;

import org.springframework.beans.factory.annotation.Required;


/**
 * Credit card number validator.
 */
public class CreditCardPaymentInfoValidator implements ValidateInterceptor
{

	private CreditCardNumberHelper creditCardNumberHelper;

	@Override
	public void onValidate(final Object model, final InterceptorContext ctx) throws InterceptorException
	{
		if (model instanceof CreditCardPaymentInfoModel)
		{
			final CreditCardPaymentInfoModel creditCardPaymentInfo = (CreditCardPaymentInfoModel) model;
			//if card number or card type was modified
			if (ctx.isModified(creditCardPaymentInfo, CreditCardPaymentInfoModel.NUMBER)
					|| ctx.isModified(creditCardPaymentInfo, CreditCardPaymentInfoModel.TYPE))
			{
				boolean valid = false;
				try
				{
					valid = creditCardNumberHelper.isValidCardNumber(creditCardPaymentInfo.getNumber(), creditCardPaymentInfo
							.getType());
				}
				catch (final BusinessException e)
				{
					throw new InterceptorException("Could not validate card number due to : " + e.getMessage());
				}
				if (!valid)
				{
					throw new InterceptorException("Credit card is invalid!");
				}
			}

		}
	}

	@Required
	public void setCreditCardNumberHelper(final CreditCardNumberHelper creditCardNumberHelper)
	{
		this.creditCardNumberHelper = creditCardNumberHelper;
	}
}
