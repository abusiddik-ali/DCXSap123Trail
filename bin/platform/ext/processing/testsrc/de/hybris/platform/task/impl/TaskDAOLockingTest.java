/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentCaptor.forClass;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.Tenant;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.tx.Transaction;

import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.dao.DataAccessException;

@IntegrationTest
public class TaskDAOLockingTest extends ServicelayerBaseTest
{
	@Test
	public void lockShouldUpdateEntitiesWithTransaction()
	{
		final MockTaskDAO taskDAO = spy(new MockTaskDAO(Registry.getCurrentTenant()));

		taskDAO.lock(1L, 1L);

		Transaction.current().begin();
		final long txObjId = Transaction.current().getObjectID();
		taskDAO.lock(2L, 2L);
		Transaction.current().commit();


		verify(taskDAO, times(2)).getLockQuery();
		verify(taskDAO, times(2)).getConditionConsumeQuery();

		final ArgumentCaptor<Boolean> txArgCaptor = forClass(Boolean.class);
		final ArgumentCaptor<Long> txObjIdCaptor = forClass(Long.class);
		verify(taskDAO, times(4)).markRunningTx(txObjIdCaptor.capture(), txArgCaptor.capture());

		assertThat(txArgCaptor.getAllValues()).allMatch(b -> b);
		assertThat(txObjIdCaptor.getAllValues()).filteredOn(aLong -> aLong.equals(txObjId)).hasSize(2);
	}

	public static class MockTaskDAO extends TaskDAO
	{

		public MockTaskDAO(final Tenant t)
		{
			super(t);
		}

		@Override
		protected int update(final String upQuery, final Object[] params) throws DataAccessException
		{
			markRunningTx(Transaction.current().getObjectID(), Transaction.current().isRunning());
			return 1;
		}

		public void markRunningTx(final long objectID, final boolean txRunning)
		{
		}
	}
}
