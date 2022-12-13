/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.internal.model.impl;

import de.hybris.platform.directpersistence.record.ModificationRecord;
import de.hybris.platform.directpersistence.record.impl.PropertyHolder;

import java.util.Collection;
import java.util.Locale;
import java.util.Optional;
import java.util.Set;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;


public class RecordAssert extends AbstractAssert<RecordAssert, ModificationRecord>
{
	public RecordAssert(final ModificationRecord actual)
	{
		super(actual, RecordAssert.class);
	}

	public static RecordAssert assertThat(final ModificationRecord actual)
	{
		return new RecordAssert(actual);
	}

	public RecordAssert isTypeOf(final String type)
	{
		Assertions.assertThat(actual.getType()).isEqualTo(type);
		return this;
	}

	public RecordAssert hasChanges()
	{
		Assertions.assertThat(actual.getChanges()).isNotNull().isNotEmpty();
		return this;
	}

	public RecordAssert hasChangesOfSize(final int size)
	{
		hasChanges();
		Assertions.assertThat(actual.getChanges()).hasSize(size);
		return this;
	}

	public RecordAssert hasLocalizedChanges()
	{
		Assertions.assertThat(actual.getLocalizedChanges()).isNotNull().isNotEmpty();
		return this;
	}

	public RecordAssert hasLocalizedChangesOfSize(final int size)
	{
		hasLocalizedChanges();
		Assertions.assertThat(actual.getLocalizedChanges()).hasSize(size);
		return this;
	}

	public RecordAssert hasPropertyHolderWithNameAndValue(final String propertyName, final Object value)
	{
		final Optional<PropertyHolder> holder = getPropertyHolder(propertyName);
		Assertions.assertThat(holder.isPresent()).isTrue();
		Assertions.assertThat(holder.get().getValue()).isEqualTo(value);
		return this;
	}

	public RecordAssert hasPropertyHolderWithNameAndValue(final String propertyName, final Object value, final Locale locale)
	{
		final Optional<PropertyHolder> holder = getPropertyHolder(propertyName, locale);
		Assertions.assertThat(holder.isPresent()).isTrue();
		Assertions.assertThat(holder.get().getValue()).isEqualTo(value);
		return this;
	}

	private Optional<PropertyHolder> getPropertyHolder(final String propertyName)
	{
		return findHolder(actual.getChanges(), propertyName);
	}

	private Optional<PropertyHolder> getPropertyHolder(final String propertyName, final Locale locale)
	{
		final Set<PropertyHolder> holders = actual.getLocalizedChanges().get(locale);
		return findHolder(holders, propertyName);
	}

	private Optional<PropertyHolder> findHolder(final Collection<PropertyHolder> holders, final String propertyName)
	{
		if (holders == null)
		{
			return Optional.empty();
		}

		return holders.stream().filter(input -> propertyName.equals(input.getName())).findAny();
	}
}
