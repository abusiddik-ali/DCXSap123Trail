/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.tenant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.Tenant;
import de.hybris.platform.servicelayer.MockTest;
import de.hybris.platform.servicelayer.model.ModelService;

import javax.annotation.Resource;

import org.apache.log4j.Logger;
import org.junit.Test;
import org.springframework.test.context.ContextConfiguration;


@UnitTest
@ContextConfiguration(locations =
		{ "classpath:/servicelayer/test/servicelayer-mock-base-test.xml" })
public class MockTenantTest extends MockTest
{
	@SuppressWarnings("unused")
	private static final Logger log = Logger.getLogger(MockTenantTest.class);


	@Resource(name = "tenantService")
	private TenantService tenantService;

	@Resource(name = "tenantFactory")
	private Tenant tenant;


	@Test
	public void testEventRegistration()
	{
		assertEquals(tenant.getTenantID(), tenantService.getCurrentTenantId());
	}

	@Test
	public void testCurrentTenant() throws Exception
	{
		assertEquals("junit", tenant.getTenantID());
		assertEquals("junit", tenantService.getCurrentTenantId());

		tenantService = (TenantService) applicationContext.getBean("tenantService");
		assertEquals("junit", tenantService.getCurrentTenantId());
	}

	@Test
	public void testOtherServicesDiffer() throws Exception
	{
		final ModelService one = (ModelService) applicationContext.getBean("modelService");
		final ModelService one2 = (ModelService) applicationContext.getBean("modelService");
		assertTrue(one == one2);
	}

}
