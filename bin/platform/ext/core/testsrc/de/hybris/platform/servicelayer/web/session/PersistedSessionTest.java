/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.web.session;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import de.hybris.platform.testframework.HybrisJUnit4TransactionalTest;

import org.apache.commons.lang3.SerializationUtils;
import org.junit.Test;


public class PersistedSessionTest extends HybrisJUnit4TransactionalTest
{
	@Test
	public void testCreatePersistedSession()
	{
		//https://jira.hybris.com/browse/ECP-2554
		final PersistedSession persistedSession = new PersistedSession("test", 99, "test", "testContext");
		persistedSession.setAttribute("dummy", null);
		persistedSession.setAttribute("dummy2", "testValue");
		final byte[] b = SerializationUtils.serialize(persistedSession);

		final PersistedSession s = SerializationUtils.deserialize(b);
		assertNull(s.getAttribute("dummy"));
		assertEquals(s.getAttribute("dummy2"), "testValue");
	}

}
