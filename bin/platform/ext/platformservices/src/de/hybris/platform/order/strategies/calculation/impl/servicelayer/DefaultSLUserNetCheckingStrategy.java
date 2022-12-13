/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.impl.servicelayer;

import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.user.UserNetCheckingStrategy;


/**
 * The default SL implementation of the {@link UserNetCheckingStrategy}.
 */
public class DefaultSLUserNetCheckingStrategy implements UserNetCheckingStrategy
{

	@Override
	public boolean isNetUser(final UserModel user)
	{
		return false;
	}

}
