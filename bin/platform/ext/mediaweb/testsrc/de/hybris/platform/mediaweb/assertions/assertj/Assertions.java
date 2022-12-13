/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.mediaweb.assertions.assertj;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.HttpStatus;

public class Assertions extends org.assertj.core.api.Assertions
{

	public static HttpStatusAssert assertThat(final HttpStatus actual)
	{
		return new HttpStatusAssert(actual);
	}


	public static HttpResponseAssert assertThat(final HttpServletResponse actual)
	{
		return new HttpResponseAssert(actual);
	}
}
