/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.processing.distributed.defaultimpl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.processing.model.DistributedProcessTransitionTaskModel;
import de.hybris.platform.processing.model.DistributedProcessWorkerTaskModel;
import de.hybris.platform.task.TaskModel;
import de.hybris.platform.task.TaskService;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;


@UnitTest
public class DispatcherUnitTest
{

	@Mock
	private TransitionsHandler transitionsHandler;

	@Mock
	private ExecutionHandler executionHandler;

	@Mock
	private TaskService taskService;

	@Mock
	private DistributedProcessLoggingCtxFactory distributedProcessLoggingCtxFactory;

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void shouldFailWhenUnsupportedTaskTypeIsRun()
	{
		final Dispatcher dispatcher = givenDispatcher();
		final TaskModel task = new TaskModel();

		try
		{
			dispatcher.run(taskService, task);
		}
		catch (final UnsupportedOperationException e)
		{
			assertThat(e).isExactlyInstanceOf(UnsupportedOperationException.class).hasNoCause();
			return;
		}
		fail("UnsupportedOperationException was expected but nothing has been thrown.");
	}

	@Test
	public void shouldFailWhenUnsupportedTaskTypeIsReportedAsFailed()
	{
		final Dispatcher dispatcher = givenDispatcher();
		final TaskModel task = new TaskModel();

		try
		{
			dispatcher.handleError(taskService, task, new Exception());
		}
		catch (final UnsupportedOperationException e)
		{
			assertThat(e).isExactlyInstanceOf(UnsupportedOperationException.class).hasNoCause();
			return;
		}
		fail("UnsupportedOperationException was expected but nothing has been thrown.");
	}

	@Test
	public void shouldUseTransitionsHandlerForExecutingTransitionTask()
	{
		final Dispatcher dispatcher = givenDispatcher();
		final DistributedProcessTransitionTaskModel task = new DistributedProcessTransitionTaskModel();

		dispatcher.run(taskService, task);

		verifyZeroInteractions(executionHandler);
		verify(transitionsHandler).runTransitionTask(task);
		verifyNoMoreInteractions(transitionsHandler);
	}

	@Test
	public void shouldUseTransitionsHandlerForHandlingErrorForTransitionTask()
	{
		final Dispatcher dispatcher = givenDispatcher();
		final DistributedProcessTransitionTaskModel task = new DistributedProcessTransitionTaskModel();

		dispatcher.handleError(taskService, task, new Exception());

		verifyZeroInteractions(executionHandler);
		verify(transitionsHandler).handleErrorDuringTransition(task);
		verifyNoMoreInteractions(transitionsHandler);
	}

	@Test
	public void shouldUseExecutionHandlerForExecutingWorkerTask()
	{
		final Dispatcher dispatcher = givenDispatcher();
		final DistributedProcessWorkerTaskModel task = new DistributedProcessWorkerTaskModel();

		dispatcher.run(taskService, task);

		verifyZeroInteractions(transitionsHandler);
		verify(executionHandler).runWorkerTask(task);
		verifyNoMoreInteractions(executionHandler);
	}

	@Test
	public void shouldUseExecutionHandlerForHandlingErrorForWorkerTask()
	{
		final Dispatcher dispatcher = givenDispatcher();
		final DistributedProcessWorkerTaskModel task = new DistributedProcessWorkerTaskModel();

		dispatcher.handleError(taskService, task, new Exception());

		verifyZeroInteractions(transitionsHandler);
		verify(executionHandler).handleErrorDuringBatchExecution(task);
		verifyNoMoreInteractions(executionHandler);
	}

	private Dispatcher givenDispatcher()
	{
		return new Dispatcher(transitionsHandler, executionHandler, distributedProcessLoggingCtxFactory);
	}
}
