/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task;

import static junit.framework.Assert.assertTrue;
import static junit.framework.Assert.fail;

import de.hybris.platform.jalo.ConsistencyCheckException;
import de.hybris.platform.jalo.user.Title;
import de.hybris.platform.jalo.user.UserManager;

import java.util.Date;


public class TxTestTaskRunner extends TestTaskRunner
{
	private Title _t;
	private Date _creationTime;

	synchronized Title getTitle()
	{
		return _t;
	}

	synchronized Date getCreationTime()
	{
		return _creationTime;
	}

	private synchronized void setRunData(final Title t, final Date d)
	{
		_t = t;
		_creationTime = d;
	}

	@Override
	public void run(final TaskService taskService, final TaskModel task) throws RetryLaterException
	{
		try
		{
			final Title t = UserManager.getInstance().createTitle("txTitle");
			assertTrue(t.isAlive());
			setRunData(t, t.getCreationTime());
		}
		catch (final ConsistencyCheckException e)
		{
			e.printStackTrace();
			fail("unexpected error " + e.getMessage());
		}
		finally
		{
			super.run(taskService, task);
		}
		throw new IllegalStateException("this should make the enclosing tx to rollback!!!");
	}
}
