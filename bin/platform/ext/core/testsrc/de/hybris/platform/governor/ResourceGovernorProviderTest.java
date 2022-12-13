/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.governor;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.Tenant;
import de.hybris.platform.core.threadregistry.RegistrableThread;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.Test;

@IntegrationTest
public class ResourceGovernorProviderTest extends ServicelayerBaseTest
{
	@Test
	public void shouldInstantiateItself()
	{
		assertThat(ResourceGovernorProvider.getInstance()).isNotNull();
	}

	@Test
	public void shouldReturnGovernorWhenTenantIsSet()
	{
		assertThat(Registry.<Tenant>getCurrentTenantNoFallback()).isNotNull();

		assertThat(ResourceGovernorProvider.getInstance().getResourceGovernor()).isNotNull();
	}

	@Test
	public void shouldReturnGovernorWhenTenantIsNotSet() throws InterruptedException
	{
		final AtomicReference<ResourceGovernor> governorHolder = new AtomicReference<>();

		final Thread t = new RegistrableThread(() -> {
			assertThat(Registry.<Tenant>getCurrentTenantNoFallback()).isNull();

			governorHolder.set(ResourceGovernorProvider.getInstance().getResourceGovernor());
		});
		t.start();

		t.join(TimeUnit.MINUTES.toMillis(1));

		assertThat(governorHolder.get()).isNotNull();
	}
}