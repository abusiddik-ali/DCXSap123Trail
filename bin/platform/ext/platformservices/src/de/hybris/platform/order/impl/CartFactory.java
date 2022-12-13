/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.impl;

import de.hybris.platform.core.model.order.CartModel;


/**
 * A {@link CartModel} factory.
 *
 * @deprecated since ages - Use the factory from the proper location -{@link de.hybris.platform.order.CartFactory}.
 */
@Deprecated(since = "ages", forRemoval = true)
public interface CartFactory extends de.hybris.platform.order.CartFactory
{
	//
}
