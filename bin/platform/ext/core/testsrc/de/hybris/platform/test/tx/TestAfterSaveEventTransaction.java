/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test.tx;

import de.hybris.platform.tx.AfterSaveListenerRegistry;
import de.hybris.platform.tx.DefaultTransaction;
import de.hybris.platform.tx.Transaction;


final class TestAfterSaveEventTransaction extends DefaultTransaction
{

	static TestAfterSaveEventTransaction install(final TestAfterSaveListenerRegistry registry)
	{
		Transaction.setTransactionFactory(new TestTxFactory(registry));
		return (TestAfterSaveEventTransaction) Transaction.current();
	}

	static void uninstall()
	{
		Transaction.unsetTransactionFactory();
		new CleanTx().activateAsCurrentTransaction();
	}

	private final AfterSaveListenerRegistry reg;

	private TestAfterSaveEventTransaction(final AfterSaveListenerRegistry reg)
	{
		this.reg = reg;
	}

	@Override
	protected AfterSaveListenerRegistry getAfterSaveEventListenerRegistry()
	{
		return reg;
	}

	private static class TestTxFactory implements TransactionFactory
	{
		private final AfterSaveListenerRegistry reg;

		TestTxFactory(final AfterSaveListenerRegistry reg)
		{
			this.reg = reg;
		}

		@Override
		public Transaction newCurrent()
		{
			return new TestAfterSaveEventTransaction(reg);
		}
	}

	private static class CleanTx extends DefaultTransaction
	{
		//
	}
}
