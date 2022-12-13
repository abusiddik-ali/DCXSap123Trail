/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.mediaweb;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.Tenant;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.mock.web.MockFilterChain;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

@IntegrationTest
public class MediaFilterContentTypeTest extends BaseMediaFilterContentTypeTest
{
	private static final Logger LOG = LoggerFactory.getLogger(MediaFilterContentTypeTest.class);

	@Override
	protected HttpServletResponse doFilter(final MockHttpServletRequest request) throws IOException, ServletException
	{
		LOG.info("calling request: {}", request);
		final MockHttpServletResponse response = new MockHttpServletResponse();
		new TestMediaFilter().doFilter(request, response, new MockFilterChain());
		return response;
	}

	private static class TestMediaFilter extends MediaFilter
	{

		private Tenant currentTenant;

		@Override
		protected void unsetCurrentTenant()
		{
			super.unsetCurrentTenant();
			if (currentTenant != null)
			{
				Registry.setCurrentTenant(currentTenant);
			}
		}

		@Override
		protected void setCurretTenantByID(final Iterable<String> mediaContext)
		{
			currentTenant = Registry.getCurrentTenant();
			super.setCurretTenantByID(mediaContext);
		}
	}
}
