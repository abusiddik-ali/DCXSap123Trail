/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.mediaweb.assertions.assertj;

import org.assertj.core.api.AbstractObjectAssert;
import org.springframework.http.HttpStatus;

public class HttpStatusAssert extends AbstractObjectAssert<HttpStatusAssert, HttpStatus>
{
	public HttpStatusAssert(final HttpStatus actual)
	{
		super(actual, HttpStatusAssert.class);
	}

	public HttpStatusAssert is1xxInformational()
	{
		isNotNull();
		if (!actual.is1xxInformational())
		{
			failWithMessage("expected %s to be 1xx", actual);
		}
		return this;
	}

	public HttpStatusAssert is2xxSuccessful()
	{
		isNotNull();
		if (!actual.is2xxSuccessful())
		{
			failWithMessage("expected %s to be 2xx", actual);
		}
		return this;
	}

	public HttpStatusAssert is3xxRedirection()
	{
		isNotNull();
		if (!actual.is3xxRedirection())
		{
			failWithMessage("expected %s to be 3xx", actual);
		}
		return this;
	}

	public HttpStatusAssert is4xxClientError()
	{
		isNotNull();
		if (!actual.is4xxClientError())
		{
			failWithMessage("expected %s to be 4xx", actual);
		}
		return this;
	}


	public HttpStatusAssert is5xxServerError()
	{
		isNotNull();
		if (!actual.is5xxServerError())
		{
			failWithMessage("expected %s to be 5xx", actual);
		}
		return this;
	}

	public HttpStatusAssert isEqualTo(final int expected)
	{
		return super.isEqualTo(HttpStatus.resolve(expected));
	}
}
