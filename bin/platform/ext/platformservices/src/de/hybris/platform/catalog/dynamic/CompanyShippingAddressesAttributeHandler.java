/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.dynamic;

import de.hybris.platform.core.model.user.AddressModel;

import java.util.Collection;
import java.util.Iterator;


public class CompanyShippingAddressesAttributeHandler extends AbstractCompanyAddressAttributeHandler
{

	@Override
	public void filterOutAddresses(final Collection<AddressModel> addresses)
	{
		for (final Iterator<AddressModel> it = addresses.iterator(); it.hasNext(); )
		{
			final AddressModel address = it.next();
			if (address.getShippingAddress() == null || !address.getShippingAddress())
			{
				it.remove();
			}
		}
	}

}
