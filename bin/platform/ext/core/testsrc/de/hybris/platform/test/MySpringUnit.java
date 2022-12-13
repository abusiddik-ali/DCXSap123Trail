/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test;

import de.hybris.platform.jalo.Item;
import de.hybris.platform.jalo.JaloBusinessException;
import de.hybris.platform.jalo.SessionContext;
import de.hybris.platform.jalo.type.ComposedType;


/**
 * For {@link ComposedTypeSpringTest} only!
 */
public class MySpringUnit extends MyUnit
{
	@Override
	protected Item createItem(final SessionContext ctx, final ComposedType type, final ItemAttributeMap allAttributes)
			throws JaloBusinessException
	{

		allAttributes.put(CODE, "<" + allAttributes.get(CODE) + ">");
		return super.createItem(ctx, type, allAttributes);
	}
}
