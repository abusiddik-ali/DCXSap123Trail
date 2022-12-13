/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.impex;

import de.hybris.platform.impex.jalo.ImpExException;
import de.hybris.platform.impex.jalo.header.HeaderValidationException;
import de.hybris.platform.impex.jalo.header.SpecialColumnDescriptor;
import de.hybris.platform.impex.jalo.header.UnresolvedValueException;
import de.hybris.platform.impex.jalo.translators.AbstractSpecialValueTranslator;
import de.hybris.platform.jalo.Item;


/**
 * Testing translator
 */
public class MyTranslator extends AbstractSpecialValueTranslator
{
	private SpecialColumnDescriptor columnDescriptor;

	@Override
	public void init(final SpecialColumnDescriptor columnDescriptor) throws HeaderValidationException
	{
		this.columnDescriptor = columnDescriptor;
	}

	@Override
	public void performImport(final String cellValue, final Item processedItem) throws ImpExException
	{
		throw new UnresolvedValueException("Foo");
	}
}
