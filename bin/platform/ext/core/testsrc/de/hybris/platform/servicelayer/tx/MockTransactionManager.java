/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.tx;

import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.TransactionException;
import org.springframework.transaction.support.AbstractPlatformTransactionManager;
import org.springframework.transaction.support.DefaultTransactionStatus;


/**
 *
 */
public class MockTransactionManager extends AbstractPlatformTransactionManager
{
	@Override
	protected void doBegin(final Object transaction, final TransactionDefinition definition) throws TransactionException
	{
		//empty
	}

	@Override
	protected void doCommit(final DefaultTransactionStatus status) throws TransactionException
	{
		//empty
	}

	@Override
	protected Object doGetTransaction() throws TransactionException
	{
		return new Object();
	}

	@Override
	protected void doRollback(final DefaultTransactionStatus status) throws TransactionException
	{
		//empty
	}

}
