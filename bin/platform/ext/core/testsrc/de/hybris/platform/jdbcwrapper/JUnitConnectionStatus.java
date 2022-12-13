/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jdbcwrapper;

import java.util.concurrent.atomic.AtomicBoolean;


public class JUnitConnectionStatus extends ConnectionStatus
{
	private final AtomicBoolean forceHasConnectionErrors = new AtomicBoolean(false);

	@Override
	public boolean hadError()
	{
		return super.hadError();
	}

	public void setPoolHasConnectionErrors(final boolean hasErrors)
	{
		forceHasConnectionErrors.set(hasErrors);
	}
}
