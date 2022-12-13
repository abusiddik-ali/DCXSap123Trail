/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.interceptor;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertTrue;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.enumeration.EnumerationValueModel;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;

import javax.annotation.Resource;

import org.junit.Test;


/**
 * test-class for EnumerationPrepareInterceptor.
 */
@IntegrationTest
public class EnumerationPrepareInterceptorTest extends ServicelayerTransactionalBaseTest
{

	@Resource
	private ModelService modelService;


	@Test
	public void testOnPrepareCorrectEnum()
	{
		final EnumerationValueModel evModel = modelService.create(OrderStatus.CREATED.getType());
		assertNotNull(evModel);
		evModel.setCode("TestValueCode");
		final Integer oldSequenceNumber = evModel.getSequenceNumber();

		modelService.save(evModel);
		assertNotNull(evModel.getSequenceNumber());

		assertEquals("TestValueCode", evModel.getCode());
		assertTrue(evModel.getSequenceNumber() != oldSequenceNumber);

	}

}
