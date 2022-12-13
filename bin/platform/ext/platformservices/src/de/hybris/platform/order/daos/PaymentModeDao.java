/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.daos;

import de.hybris.platform.core.model.order.payment.PaymentModeModel;
import de.hybris.platform.servicelayer.internal.dao.GenericDao;

import java.util.List;


/**
 * Data Access Object oriented on PaymentMode
 */
public interface PaymentModeDao extends GenericDao<PaymentModeModel>
{
	/**
	 * Search for PaymentMode by code
	 *
	 * @param code the code
	 */
	List<PaymentModeModel> findPaymentModeForCode(final String code);

	/**
	 * Search for all PaymentModes
	 */
	List<PaymentModeModel> findAllPaymentModes();


}
