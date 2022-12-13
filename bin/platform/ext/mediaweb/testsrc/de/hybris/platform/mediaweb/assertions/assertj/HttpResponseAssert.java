/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.mediaweb.assertions.assertj;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Objects;

import javax.servlet.http.HttpServletResponse;

import org.assertj.core.api.AbstractObjectAssert;
import org.springframework.http.HttpStatus;

public class HttpResponseAssert extends AbstractObjectAssert<HttpResponseAssert, HttpServletResponse>
{
	public HttpResponseAssert(final HttpServletResponse actual)
	{
		super(actual, HttpResponseAssert.class);
	}

	public HttpStatusAssert status()
	{
		assertThat(actual).isNotNull();
		return new HttpStatusAssert(HttpStatus.valueOf(actual.getStatus()));
	}

	public HttpResponseAssert headerIsEqualTo(final String header, final String value)
	{
		assertThat(actual).isNotNull().extracting(s -> s.getHeader(header)).isEqualTo(value);
		return this;
	}

	public HttpResponseAssert headerContains(final String header, final String... values)
	{
		assertThat(actual).isNotNull().extracting(s -> s.getHeaders(header)).contains(Objects.requireNonNull(values));
		return this;
	}


	public HttpResponseAssert headerContainsOnly(final String header, final String... values)
	{
		assertThat(actual).isNotNull().extracting(s -> s.getHeaders(header)).containsOnly(Objects.requireNonNull(values));
		return this;
	}

	public HttpResponseAssert hasContentType(final String contentType)
	{
		assertThat(actual.getContentType()).isEqualToIgnoringCase(contentType);
		return this;
	}

}
