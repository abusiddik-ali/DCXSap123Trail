/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.impl;

import de.hybris.platform.catalog.model.SyncAttributeDescriptorConfigModel;
import de.hybris.platform.core.model.type.AttributeDescriptorModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;


/**
 * {@link PrepareInterceptor} for the {@link SyncAttributeDescriptorConfigModel}.
 */
public class SyncAttributeDescriptorConfigPreparer implements PrepareInterceptor
{
	@Override
	public void onPrepare(final Object model, final InterceptorContext ctx) throws InterceptorException
	{
		if (model instanceof SyncAttributeDescriptorConfigModel)
		{
			final SyncAttributeDescriptorConfigModel modelImpl = (SyncAttributeDescriptorConfigModel) model;
			modelImpl.setCopyByValue(adjustCopyByValueImpl(modelImpl.getAttributeDescriptor(), modelImpl.getCopyByValue()));
		}
	}

	private Boolean adjustCopyByValueImpl(final AttributeDescriptorModel adm, final Boolean toSet)
	{
		Boolean result = Boolean.FALSE;
		if (adm != null && adm.getPartOf() != null)
		{
			result = adm.getPartOf().booleanValue() || Boolean.TRUE.equals(toSet) ? Boolean.TRUE : Boolean.FALSE;
		}
		return result;
	}
}
