/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.event.impl;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.spring.ctx.CloseAwareApplicationContext;

import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;


/**
 *
 */
@UnitTest
public class ContextRefreshedEventListenerTest
{


	@Test
	public void testNoRegisterForNonCloseAwareApplicationContext()
	{

		final ApplicationContext ctx = Mockito.mock(ApplicationContext.class);
		final ContextRefreshedEvent event = new ContextRefreshedEvent(ctx);


		final ApplicationListener<ContextRefreshedEvent> listener = new ContextRefreshedEventListener();

		listener.onApplicationEvent(event);


		Mockito.verifyZeroInteractions(ctx);
	}

	@Test
	public void testRegisterForCloseAwareApplicationContext()
	{

		final ApplicationContext ctx = Mockito.mock(CloseAwareApplicationContext.class);
		final PlatformClusterEventSender sender = Mockito.mock(PlatformClusterEventSender.class);

		final ContextRefreshedEvent event = new ContextRefreshedEvent(ctx);

		Mockito.when(ctx.getBean("platformClusterEventSender", PlatformClusterEventSender.class)).thenReturn(sender);

		final ApplicationListener<ContextRefreshedEvent> listener = new ContextRefreshedEventListener();

		listener.onApplicationEvent(event);


		Mockito.verify(sender, Mockito.times(1)).registerBinaryListenerHook();
	}
}
