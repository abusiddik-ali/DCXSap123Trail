/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order;

import de.hybris.platform.core.model.order.payment.PaymentModeModel;

import java.util.List;


/**
 * Service around the {@link PaymentModeModel}. {@link PaymentModeModel} is used in calculation and store informations
 * about payment on order.
 *
 * @spring.bean paymentModeService
 */
public interface PaymentModeService
{

	/**
	 * Gets the {@link PaymentModeModel} with the specified code.
	 *
	 * @param code the code
	 * @return the found list of {@link PaymentModeModel} with the specified code
	 */
	PaymentModeModel getPaymentModeForCode(final String code);

	/**
	 * Gets all payment modes
	 */
	List<PaymentModeModel> getAllPaymentModes();


}
