/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.search;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.fail;

import de.hybris.bootstrap.annotations.UnitTest;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;


@UnitTest
public class TranslationResultTest
{
	@Test
	public void shouldSetResultListAsLocalUnmodifiableCopyInsteadOfUsingReference()
	{
		// given
		final List<Object> unsafeList = new ArrayList<Object>();
		unsafeList.add("Foo");
		unsafeList.add("Bar");

		// when
		final TranslationResult tResult = new TranslationResult("SELECT {PK} FROM {Product}", unsafeList);
		unsafeList.clear();

		// then
		assertThat(unsafeList).isEmpty();
		assertThat(tResult.getSQLQueryParameters()).isNotEmpty();
		assertThat(tResult.getSQLQueryParameters()).hasSize(2);
		try
		{
			tResult.getSQLQueryParameters().add("Baz");
			fail("Should throw UnsupportedOperationException");
		}
		catch (final UnsupportedOperationException e)
		{
			// that's OK
		}
	}
}
