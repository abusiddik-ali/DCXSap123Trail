/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.processengine.actionstrategy;

import de.hybris.platform.processengine.action.AbstractProceduralAction;
import de.hybris.platform.processengine.model.BusinessProcessModel;
import de.hybris.platform.task.RetryLaterException;


public class ThrowErrorAction extends AbstractProceduralAction
{
	public int calls = 0;
	public BusinessProcessModel process;

	@Override
	public void executeAction(final BusinessProcessModel process) throws RetryLaterException, Exception
	{
		throw new Exception("error action throws exception");
	}
}
