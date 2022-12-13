/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core.threadregistry;

import de.hybris.platform.core.threadregistry.OperationInfo.StandardAttributes;

import java.util.Objects;


class TestSupport
{
	private final ThreadRegistry threadRegistry;

	public static TestSupport forRegistry(final ThreadRegistry threadRegistry)
	{
		return new TestSupport(threadRegistry);
	}

	private TestSupport(final ThreadRegistry threadRegistry)
	{
		this.threadRegistry = Objects.requireNonNull(threadRegistry);
	}

	public boolean isCurrentThreadRegistered()
	{
		return getCurrentOperationInfo() != null;
	}

	public Object getAttributeFromCurrentOperation(final StandardAttributes attribute)
	{
		return getCurrentOperationInfo().getAllAttributes().get(attribute);
	}

	public String getAttributeFromCurrentOperation(final String attribute)
	{
		return (String) getCurrentOperationInfo().getAllAttributes().get(attribute);
	}

	public OperationInfo getCurrentOperationInfo()
	{
		return threadRegistry.getAllOperations().get(Long.valueOf(Thread.currentThread().getId()));
	}
}
