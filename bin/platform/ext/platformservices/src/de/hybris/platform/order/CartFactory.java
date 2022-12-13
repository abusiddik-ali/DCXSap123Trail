/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order;

import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.order.impl.DefaultCartFactory;


/**
 * Factory for the cart type. Implementation should create {@link CartModel}. Default implementation for instance (
 * {@link DefaultCartFactory} ), creates an instance of {@link CartModel} that is supposed to be the session cart. So it
 * populates the user and currency fields according to the current session status and persists the created model.
 */
public interface CartFactory
{
	/**
	 * Creates a new, implementation dependent {@link CartModel} instance.
	 *
	 * @return {@link CartModel} - may be persisted or not, it depends on the used implementation. I.e
	 * {@link DefaultCartFactory} saves the model.
	 */
	CartModel createCart();
}
