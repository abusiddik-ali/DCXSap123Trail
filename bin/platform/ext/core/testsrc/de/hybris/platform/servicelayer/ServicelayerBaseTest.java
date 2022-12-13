/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer;

import de.hybris.platform.testframework.HybrisJUnit4Test;

import java.lang.reflect.Field;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Ignore;
import org.springframework.context.ApplicationContext;


public abstract class ServicelayerBaseTest extends HybrisJUnit4Test
{
	private final ServicelayerBaseTestLogic servicelayerBaseTestLogic = new ServicelayerBaseTestLogic();

	@Before
	public void prepareApplicationContextAndSession() throws Exception
	{
		servicelayerBaseTestLogic.prepareApplicationContextAndSession(this);
	}

	protected void autowireProperties(final ApplicationContext applicationContext)
	{
		servicelayerBaseTestLogic.autowireProperties(applicationContext, this);
	}

	protected String getBeanName(final Resource resource, final Field field)
	{
		return servicelayerBaseTestLogic.getBeanName(resource, field);
	}

	protected ApplicationContext getApplicationContext()
	{
		return servicelayerBaseTestLogic.getApplicationContext();
	}
}
