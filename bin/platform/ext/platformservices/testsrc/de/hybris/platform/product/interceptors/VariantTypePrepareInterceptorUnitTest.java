/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.product.interceptors;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.type.TypeService;
import de.hybris.platform.variants.model.VariantTypeModel;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;


@UnitTest
public class VariantTypePrepareInterceptorUnitTest
{
	@Mock
	private TypeService typeService;

	private final VariantTypePrepareInterceptor interceptor = new VariantTypePrepareInterceptor();

	@Before
	public void prepare()
	{
		MockitoAnnotations.initMocks(this);
		interceptor.setTypeService(typeService);
	}

	@Test
	public void testInterceptNullModel() throws InterceptorException
	{
		interceptor.onPrepare(null, null);
	}

	@Test
	public void testInterceptNotCompatibleModel() throws InterceptorException
	{
		final ProductModel model = Mockito.mock(ProductModel.class);

		interceptor.onPrepare(model, null);

		Mockito.verifyZeroInteractions(model);
	}

	@Test
	public void testInterceptNotNewModel() throws InterceptorException
	{
		final VariantTypeModel model = Mockito.mock(VariantTypeModel.class);
		final InterceptorContext ctx = Mockito.mock(InterceptorContext.class);
		Mockito.when(Boolean.valueOf(ctx.isNew(model))).thenReturn(Boolean.TRUE);
		interceptor.onPrepare(model, ctx);

		Mockito.verify(model).getSuperType();
	}
}
