/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.cluster.jgroups;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import java.net.UnknownHostException;

import org.junit.Test;

@IntegrationTest
public class JGroupsBroadcastMethodTest extends ServicelayerBaseTest
{

	@Test(expected = UnknownHostException.class)
	public void shouldThrowExceptionForInvalidBindAddress() throws Exception
	{
		final JGroupsBroadcastMethod jGroupBroadcastMethod = new JGroupsBroadcastMethod();
		jGroupBroadcastMethod.validateBindAddressConfiguration("1237.0.0.1", "8080");
	}

	@Test
	public void shouldValidateBindAddress() throws Exception
	{
		final JGroupsBroadcastMethod jGroupBroadcastMethod = new JGroupsBroadcastMethod();
		jGroupBroadcastMethod.validateBindAddressConfiguration("127.0.0.1", "8080");
	}

	@Test
	public void shouldValidateMatchInterface() throws Exception
	{
		final JGroupsBroadcastMethod jGroupBroadcastMethod = new JGroupsBroadcastMethod();
		jGroupBroadcastMethod.validateBindAddressConfiguration("match-interface:en5", null);
	}

	@Test(expected = IllegalArgumentException.class)
	public void shouldThrowExceptionForInvalidMatchInterface() throws Exception
	{
		final JGroupsBroadcastMethod jGroupBroadcastMethod = new JGroupsBroadcastMethod();
		jGroupBroadcastMethod.validateBindAddressConfiguration("match-iinterface:en5", null);
	}

}
