/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.spring.ctx;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import java.util.Map;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.xml.XmlBeanDefinitionReader;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.core.io.ClassPathResource;


@IntegrationTest
public class ScopeTenantIgnoreDocReaderTest extends ServicelayerBaseTest
{

	protected static final String LAZY_INIT_FALSE_BEAN = "lazyInitFalseBean";
	protected static final String LAZY_INIT_TRUE_BEAN = "lazyInitTrueBean";
	protected static final String LAZY_INIT_DEFAULT_BEAN = "lazyInitDefaultBean";

	private Map creationRegistry;

	@Before
	public void doBefore()
	{
		prepare();
	}

	@After
	public void doAfter()
	{
		if (creationRegistry != null)
		{
			creationRegistry.clear();
			creationRegistry = null;
		}
	}

	protected void prepare()
	{

		final org.springframework.core.io.Resource lazyInitResource = new ClassPathResource("test/lazy-init-test-spring.xml",
				Registry.class.getClassLoader());


		final GenericApplicationContext context = new GenericApplicationContext();


		final XmlBeanDefinitionReader xmlReader = new XmlBeanDefinitionReader(context);
		xmlReader.setDocumentReaderClass(getDocReaderClass());
		xmlReader.loadBeanDefinitions(lazyInitResource);

		context.refresh();

		creationRegistry = context.getBean("creationRegistry", Map.class);
	}

	protected Class<? extends ScopeTenantIgnoreDocReader> getDocReaderClass()
	{
		return ScopeTenantIgnoreDocReader.class;
	}


	@Test
	public void testCreation()
	{
		//
		assertBeanCreated(LAZY_INIT_DEFAULT_BEAN);
		assertBeanNotCreated(LAZY_INIT_TRUE_BEAN);
		assertBeanCreated(LAZY_INIT_FALSE_BEAN);
	}

	protected void assertBeanCreated(final String beanid)
	{
		Assert.assertNotNull("Bean of id " + beanid + " should be created", creationRegistry.get(beanid));
	}

	protected void assertBeanNotCreated(final String beanid)
	{
		Assert.assertNull("Bean of id " + beanid + " should not be created", creationRegistry.get(beanid));
	}
}
