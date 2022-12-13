/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.synchronization;

import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.Map;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;


public class ProductAssert extends AbstractAssert<ProductAssert, ProductModel>
{
	private final ModelService modelService;

	protected ProductAssert(final ProductModel actual)
	{
		super(actual, ProductAssert.class);
		modelService = Registry.getApplicationContext().getBean("modelService", ModelService.class);
	}

	public static ProductAssert assertThat(final ProductModel protuct)
	{
		return new ProductAssert(protuct);
	}

	public ProductAssert hasPropertyValuesAs(final Map<String, String> values)
	{
		for (final Map.Entry<String, String> entry : values.entrySet())
		{
			final Object actualValue = modelService.getAttributeValue(actual, entry.getKey());
			Assertions.assertThat(actualValue).isInstanceOf(String.class);
			Assertions.assertThat((String) actualValue).contains(entry.getValue());
		}

		return this;
	}
}
