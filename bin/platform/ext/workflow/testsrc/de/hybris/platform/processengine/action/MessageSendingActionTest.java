/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.processengine.action;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.processengine.model.BusinessProcessModel;
import de.hybris.platform.servicelayer.event.events.MessageSendingException;
import de.hybris.platform.task.RetryLaterException;

import org.assertj.core.api.Assertions;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageChannel;


@UnitTest
public class MessageSendingActionTest
{
	@InjectMocks
	private final MessageSendingAction messageSendingAction = new MessageSendingAction();

	@Mock
	MessageChannel channel;

	@Before
	public void setUp()
	{
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void testExecute() throws RetryLaterException, Exception
	{
		final BusinessProcessModel process = new BusinessProcessModel();
		Mockito.when(channel.send(Mockito.anyObject())).thenReturn(Boolean.TRUE);
		messageSendingAction.execute(process);
	}

	@Test
	public void testExecuteError() throws RetryLaterException, Exception
	{
		final BusinessProcessModel process = new BusinessProcessModel();
		Mockito.when(channel.send(Mockito.anyObject())).thenReturn(Boolean.FALSE);
		Assertions.assertThatThrownBy(() -> messageSendingAction.execute(process)).isInstanceOf(MessageSendingException.class);
	}
}
