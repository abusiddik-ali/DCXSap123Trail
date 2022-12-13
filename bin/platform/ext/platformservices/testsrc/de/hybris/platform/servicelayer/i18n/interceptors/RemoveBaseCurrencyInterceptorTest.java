/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.i18n.interceptors;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;


/**
 * Test covering the (@link RemoveBaseCurrencyInterceptor) logic.
 */
@UnitTest
public class RemoveBaseCurrencyInterceptorTest
{

	@Mock
	private InterceptorContext interceptorContext;
	private RemoveBaseCurrencyInterceptor interceptor;

	@Before
	public void setUp()
	{
		MockitoAnnotations.initMocks(this);

		interceptor = new RemoveBaseCurrencyInterceptor();
	}

	@Test(expected = InterceptorException.class)
	public void testRemoveInterceptorBaseCurrencyTrue() throws InterceptorException
	{
		final CurrencyModel curr = new CurrencyModel();
		curr.setBase(Boolean.TRUE);
		interceptor.onRemove(curr, interceptorContext);
	}


	@Test
	public void testRemoveInterceptorBaseCurrencyFalse() throws InterceptorException
	{
		final CurrencyModel curr = new CurrencyModel();
		curr.setBase(Boolean.FALSE);
		interceptor.onRemove(curr, interceptorContext);
	}
}
