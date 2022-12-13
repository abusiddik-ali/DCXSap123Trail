/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.impex.jalo.imp;

import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;


/**
 *
 */
public class TestAddressModelValidateInterceptor implements ValidateInterceptor
{
	private volatile String uidToGetActiveFor;
	private volatile boolean exceptionThrown = false;

	@Override
	public void onValidate(final Object addressModel, final InterceptorContext ctx) throws InterceptorException
	{
		if (shouldTrigger(addressModel))
		{
			if (((AddressModel) addressModel).getTitle() == null)
			{
				exceptionThrown = true;
				throw new InterceptorException("Can not create address without title");
			}
		}
	}

	private boolean shouldTrigger(final Object addressModel)
	{
		return uidToGetActiveFor != null && addressModel instanceof AddressModel
				&& ((AddressModel) addressModel).getOwner() instanceof CustomerModel
				&& ((CustomerModel) ((AddressModel) addressModel).getOwner()).getUid().equals(getUidToGetActiveFor());
	}

	public String getUidToGetActiveFor()
	{
		return uidToGetActiveFor;
	}

	public boolean wasExceptionThrown()
	{
		return exceptionThrown;
	}

	public void setUpForTest(final String uidToGetActiveFor)
	{
		this.uidToGetActiveFor = uidToGetActiveFor;
		this.exceptionThrown = false;
	}

	public void deactivate()
	{
		uidToGetActiveFor = null;
	}
}
