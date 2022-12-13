/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt.converter;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;


public interface PDTConverter<SOURCE, TARGET, CONTEXT>
{
	TARGET convert(SOURCE source, CONTEXT context);

	default List<TARGET> convertAll(final Collection<? extends SOURCE> sources, final CONTEXT context)
	{
		if (CollectionUtils.isEmpty(sources))
		{
			return Collections.emptyList();
		}

		final List<TARGET> result = new ArrayList<>(sources.size());

		for (final SOURCE source : sources)
		{
			result.add(convert(source, context));
		}
		return result;
	}
}
