/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.web;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.servicelayer.web.PolyglotPersistenceCallbackFilter.CallbackCaller;
import de.hybris.platform.servicelayer.web.PolyglotPersistenceCallbackFilter.PolyglotCallback;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;


@UnitTest
public class PolyglotPersistenceCallbackFilterTest
{

	@Mock
	private ServletRequest request;

	@Mock
	private ServletResponse response;

	@Mock
	private FilterChain filterChain;

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void shouldThrowExceptionWhenNoCallbackListSpecified()
	{
		assertThatThrownBy(() -> new PolyglotPersistenceCallbackFilter(null)).isNotNull()
		                                                                     .isInstanceOf(NullPointerException.class);
	}

	@Test
	public void shouldCallFilterChainWhenEmptyCallbackList() throws IOException, ServletException
	{
		final PolyglotPersistenceCallbackFilter filter = spy(new PolyglotPersistenceCallbackFilter(Collections
				.emptyList()));

		filter.doFilter(request, response, filterChain);

		verify(filterChain).doFilter(request, response);
	}

	@Test
	public void shouldCallCallbacksSpecified() throws IOException, ServletException
	{

		final PolyglotCallback callback = spy(new NextCallPolyglotCallback());

		final PolyglotPersistenceCallbackFilter filter = spy(new PolyglotPersistenceCallbackFilter(List.of(callback)));

		filter.doFilter(request, response, filterChain);


		verify(filterChain).doFilter(request, response);
		verify(callback).call(same(request), same(response), any());
	}

	@Test
	public void shouldCallCallbacksInGivenOrder() throws IOException, ServletException
	{

		final List<String> beforeCallbacks = new ArrayList<>();
		final List<String> afterCallbacks = new ArrayList<>();

		final Consumer<String> beforeAction = beforeCallbacks::add;
		final Consumer<String> afterAction = afterCallbacks::add;

		final PolyglotCallback callback1 = spy(new SimplePolyglotCallback("callback1", beforeAction, afterAction));
		final PolyglotCallback callback2 = spy(new SimplePolyglotCallback("callback2", beforeAction, afterAction));
		final PolyglotCallback callback3 = spy(new SimplePolyglotCallback("callback3", beforeAction, afterAction));
		final PolyglotCallback callback4 = spy(new SimplePolyglotCallback("callback4", beforeAction, afterAction));

		final List<PolyglotCallback> callbacks = List.of(callback1, callback2, callback3, callback4);
		final PolyglotPersistenceCallbackFilter filter = spy(new PolyglotPersistenceCallbackFilter(callbacks));

		filter.doFilter(request, response, filterChain);

		verify(filterChain).doFilter(request, response);

		verify(callback1).call(same(request), same(response), any());
		verify(callback2).call(same(request), same(response), any());
		verify(callback3).call(same(request), same(response), any());
		verify(callback4).call(same(request), same(response), any());

		assertThat(beforeCallbacks).hasSize(4).containsExactly("callback1", "callback2", "callback3", "callback4");
		assertThat(afterCallbacks).hasSize(4).containsExactly("callback4", "callback3", "callback2", "callback1");
	}

	@Test
	public void shouldBreakFilterChainWhenCallbackDoNotCallNextCallback() throws IOException, ServletException
	{
		final PolyglotCallback callback1 = spy(new NoNextCallPolyglotCallback());

		final PolyglotPersistenceCallbackFilter filter = spy(new PolyglotPersistenceCallbackFilter(List.of(callback1)));

		filter.doFilter(request, response, filterChain);

		verify(filterChain, never()).doFilter(request, response);
		verify(callback1).call(same(request), same(response), any());
	}

	public static class NextCallPolyglotCallback implements PolyglotCallback
	{

		@Override
		public void call(final ServletRequest servletRequest, final ServletResponse servletResponse,
		                 final CallbackCaller nextCaller) throws IOException, ServletException
		{
			nextCaller.call(servletRequest, servletResponse);
		}
	}


	public static class SimplePolyglotCallback implements PolyglotCallback
	{
		private final String name;
		private final Consumer<String> doBeforeAction;
		private final Consumer<String> doAfterAction;

		private SimplePolyglotCallback(final String name, final Consumer<String> doBeforeAction,
		                               final Consumer<String> doAfterAction)
		{
			this.name = name;
			this.doBeforeAction = doBeforeAction;
			this.doAfterAction = doAfterAction;
		}

		@Override
		public void call(final ServletRequest servletRequest, final ServletResponse servletResponse,
		                 final CallbackCaller nextCaller) throws IOException, ServletException
		{
			doBeforeAction.accept(name);

			nextCaller.call(servletRequest, servletResponse);

			doAfterAction.accept(name);
		}
	}


	public static class NoNextCallPolyglotCallback implements PolyglotCallback
	{

		@Override
		public void call(final ServletRequest servletRequest, final ServletResponse servletResponse,
		                 final CallbackCaller nextCaller)
		{

		}
	}
}
