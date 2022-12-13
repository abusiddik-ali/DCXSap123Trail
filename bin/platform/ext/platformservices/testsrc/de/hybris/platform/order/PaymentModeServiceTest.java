/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order;


import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.order.payment.PaymentModeModel;
import de.hybris.platform.order.daos.PaymentModeDao;
import de.hybris.platform.order.impl.DefaultPaymentModeService;
import de.hybris.platform.servicelayer.exceptions.AmbiguousIdentifierException;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;


@UnitTest
public class PaymentModeServiceTest
{
	private DefaultPaymentModeService defaultPaymentModeService;

	@Mock
	private PaymentModeDao paymentModeDao;

	@Before
	public void setUp()
	{
		MockitoAnnotations.initMocks(this);
		defaultPaymentModeService = new DefaultPaymentModeService();
		defaultPaymentModeService.setPaymentModeDao(paymentModeDao);

	}

	@Test
	public void testFindPaymentModeByCodeDublicatedError()
	{
		final String code = "code";
		final PaymentModeModel paymentModeModel1 = new PaymentModeModel();
		final PaymentModeModel paymentModeModel2 = new PaymentModeModel();

		Mockito.when(paymentModeDao.findPaymentModeForCode(code)).thenReturn(Arrays.asList(paymentModeModel1, paymentModeModel2));

		assertThatThrownBy(() -> defaultPaymentModeService.getPaymentModeForCode(code)).isInstanceOf(
				AmbiguousIdentifierException.class);

	}

	@Test
	public void testFindPaymentModeByCode()
	{
		final String code = "code";
		final PaymentModeModel paymentModeModel1 = new PaymentModeModel();


		Mockito.when(paymentModeDao.findPaymentModeForCode(code)).thenReturn(Collections.singletonList(paymentModeModel1));

		assertThat(paymentModeModel1).isEqualTo(defaultPaymentModeService.getPaymentModeForCode(code));

	}


	@Test
	public void testFindPaymentModeByCodeError()
	{
		final String code = "code";

		Mockito.when(paymentModeDao.findPaymentModeForCode(code)).thenReturn(new ArrayList<PaymentModeModel>());

		assertThatThrownBy(() -> defaultPaymentModeService.getPaymentModeForCode(code)).isInstanceOf(
				UnknownIdentifierException.class);
	}

	@Test
	public void testFindPaymentModeByCodeNullCode()
	{
		assertThatThrownBy(() -> defaultPaymentModeService.getPaymentModeForCode(null)).isInstanceOf(
				IllegalArgumentException.class);
	}


	@Test
	public void testGetAllPaymentModes()
	{

		final PaymentModeModel paymentModeModel1 = new PaymentModeModel();
		final PaymentModeModel paymentModeModel2 = new PaymentModeModel();


		Mockito.when(paymentModeDao.findAllPaymentModes()).thenReturn(Arrays.asList(paymentModeModel1, paymentModeModel2));

		final List<PaymentModeModel> res = defaultPaymentModeService.getAllPaymentModes();

		assertThat(res).containsExactly(paymentModeModel1, paymentModeModel2);

	}

}
