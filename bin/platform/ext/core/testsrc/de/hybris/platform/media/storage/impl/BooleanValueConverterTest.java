/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.media.storage.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;

import org.junit.Test;

@UnitTest
public class BooleanValueConverterTest
{
	private final BooleanValueConverter converter = new BooleanValueConverter();

	@Test
	public void shouldConvertStringRepresentationOfTrueIntoBooleanTrue()
	{
		// given
		final String input = "true";

		// when
		final Boolean converted = converter.convert(input);

		// then
		assertThat(converted).isNotNull().isEqualTo(Boolean.TRUE);
	}

	@Test
	public void shouldConvertNullInputIntoBooleanFalse()
	{
		// given
		final String input = null;

		// when
		final Boolean converted = converter.convert(input);

		// then
		assertThat(converted).isNotNull().isEqualTo(Boolean.FALSE);
	}

	@Test
	public void shouldConvertEmptyStringInputIntoBooleanFalse()
	{
		// given
		final String input = " ";

		// when
		final Boolean converted = converter.convert(input);

		// then
		assertThat(converted).isNotNull().isEqualTo(Boolean.FALSE);
	}

	@Test
	public void shouldConvertNotValidTrueStringRepresentationIntoBooleanFalse()
	{
		// given
		final String input = "foobar";

		// when
		final Boolean converted = converter.convert(input);

		// then
		assertThat(converted).isNotNull().isEqualTo(Boolean.FALSE);
	}

}
