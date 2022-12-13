/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.impex.jalo;

import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;


/**
 * For {@link PLA_12491_Test}.
 */
public class PLA_12491_Test_Interceptor implements ValidateInterceptor<TitleModel>
{
	private volatile String codeToThrowErrorOn;
	private volatile boolean errorThrown;

	@Override
	public void onValidate(final TitleModel model, final InterceptorContext ctx) throws InterceptorException
	{
		if (codeToThrowErrorOn != null && codeToThrowErrorOn.equalsIgnoreCase(model.getCode()))
		{
			errorThrown = true;
			//due to HORST-1477 we can not throw InterceptorException here
			throw new RuntimeException("intentional runtime error inside error - PLA-12491");
		}
	}

	public void setUpForTest(final String codeToThrowErrorOn)
	{
		this.codeToThrowErrorOn = codeToThrowErrorOn;
	}

	public boolean errorWasThrown()
	{
		return errorThrown;
	}

	public void reset()
	{
		this.codeToThrowErrorOn = null;
		this.errorThrown = false;
	}

}
