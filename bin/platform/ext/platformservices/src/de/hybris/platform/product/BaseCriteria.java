/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.product;

import de.hybris.platform.core.model.product.ProductModel;

import java.util.Date;


/**
 * BaseCriteria exposes basic attributes shared by all interfaces that subclass BaseCriteria
 */
public interface BaseCriteria
{
	ProductModel getProduct();

	Date getDate();

	Boolean isNet();
}
