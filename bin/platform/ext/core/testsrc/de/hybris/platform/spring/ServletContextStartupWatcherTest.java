/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.spring;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;

import javax.servlet.ServletContextEvent;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.mockito.runners.MockitoJUnitRunner;
import org.mockito.stubbing.Answer;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class ServletContextStartupWatcherTest
{

	@Test
	public void shouldCheckIfAnyContextInitializationIsInProgress()
	{
		//given
		final HybrisContextLoaderListenerMock hybrisContextLoaderListener = Mockito.mock(HybrisContextLoaderListenerMock.class);
		final ServletContextEvent servletContextEvent = Mockito.mock(ServletContextEvent.class);
		Mockito.doCallRealMethod().when(hybrisContextLoaderListener).contextInitialized(Mockito.any());

		final Answer answer = invocation -> {
			assertThat(ServletContextStartupWatcher.getStartupServletContextWatcher()
			                                       .isAnyServletContextInitializationInProgress()).isTrue();
			return null;
		};

		Mockito.doAnswer(answer).when(hybrisContextLoaderListener).contextInitializedInternal(Mockito.any());
		//when
		hybrisContextLoaderListener.contextInitialized(servletContextEvent);
		//then
		assertThat(ServletContextStartupWatcher.getStartupServletContextWatcher()
		                                       .isAnyServletContextInitializationInProgress()).isFalse();

	}

	public static class HybrisContextLoaderListenerMock extends HybrisContextLoaderListener
	{

		@Override
		public void contextInitializedInternal(final ServletContextEvent event)
		{
			//overwritten for visibility
		}
	}


}
