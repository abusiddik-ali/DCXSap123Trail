/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.mediaweb;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.web.WebAppMediaFilter;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.mock.web.MockFilterChain;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

@IntegrationTest
public class WebAppMediaFilterContentTypeTest extends BaseMediaFilterContentTypeTest
{
	private static final Logger LOG = LoggerFactory.getLogger(WebAppMediaFilterContentTypeTest.class);
	private final MockFilterChain chain = new MockFilterChain();
	private final WebAppMediaFilter filter = new WebAppMediaFilter();

	@Override
	protected HttpServletResponse doFilter(final MockHttpServletRequest request) throws IOException, ServletException
	{
		LOG.info("calling request: {}", request);
		final MockHttpServletResponse response = new MockHttpServletResponse();
		filter.doFilter(request, response, chain);
		return response;
	}
}
