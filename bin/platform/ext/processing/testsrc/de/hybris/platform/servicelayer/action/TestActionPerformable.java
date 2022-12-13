/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.action;

import de.hybris.platform.servicelayer.action.impl.ActionPerformable;
import de.hybris.platform.servicelayer.model.action.AbstractActionModel;

import java.util.concurrent.atomic.AtomicInteger;


public class TestActionPerformable implements ActionPerformable<String>
{
	private final AtomicInteger calls = new AtomicInteger(0);
	private AbstractActionModel action;
	private String argument;

	synchronized void setRunData(final AbstractActionModel action, final String argument)
	{
		this.action = action;
		this.argument = argument;
	}

	public int getCalls()
	{
		return calls.get();
	}

	public synchronized AbstractActionModel getAction()
	{
		return action;
	}

	public synchronized String getArgument()
	{
		return argument;
	}


	@Override
	public void performAction(final AbstractActionModel action, final String argument)
	{
		this.calls.incrementAndGet();
		setRunData(action, argument);
	}

}
