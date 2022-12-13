/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jalo.flexiblesearch.limit.impl;

import de.hybris.platform.jalo.flexiblesearch.limit.LimitStatementBuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;


/**
 * Assertion for easy checking <code>LimitStatementBuilder</code> implementations.
 */
public class LimitStatementBuilderAssert extends AbstractAssert<LimitStatementBuilderAssert, LimitStatementBuilder>
{
	public LimitStatementBuilderAssert(final LimitStatementBuilder actual)
	{
		super(actual, LimitStatementBuilderAssert.class);
	}

	public static LimitStatementBuilderAssert assertThat(final LimitStatementBuilder actual)
	{
		return new LimitStatementBuilderAssert(actual);
	}

	public LimitStatementBuilderAssert hasOriginalStartAndCountValues(final Integer val1, final Integer val2)
	{
		Assertions.assertThat(actual).isNotNull();
		Assertions.assertThat(actual.getOriginalStart()).isEqualTo(val1);
		Assertions.assertThat(actual.getOriginalCount()).isEqualTo(val2);
		return this;
	}

	public LimitStatementBuilderAssert hasAdditionalStatementValues(final Integer... values)
	{
		final List<Object> expected = new ArrayList<Object>();
		expected.add("foo");
		expected.add("bar");
		expected.addAll(Arrays.asList(values));

		Assertions.assertThat(actual).isNotNull();
		Assertions.assertThat(actual.getModifiedStatementValues()).hasSize(2 + values.length);
		Assertions.assertThat(actual.getModifiedStatementValues()).isEqualTo(expected);
		return this;
	}

	public LimitStatementBuilderAssert hasNoAdditionalStatementValues()
	{
		Assertions.assertThat(actual).isNotNull();
		Assertions.assertThat(actual.getModifiedStatementValues()).hasSize(2);
		return this;
	}
}
