/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.directpersistence.read;

import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.directpersistence.cache.SLDDataContainer;
import de.hybris.platform.util.ItemPropertyValue;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;


public class AttributeValueAssert extends AbstractAssert<AttributeValueAssert, SLDDataContainer.AttributeValue>
{

	public AttributeValueAssert(final SLDDataContainer.AttributeValue actual)
	{
		super(actual, AttributeValueAssert.class);
	}

	public static AttributeValueAssert assertThat(final SLDDataContainer.AttributeValue actual)
	{
		return new AttributeValueAssert(actual);
	}

	public AttributeValueAssert withValueEqualTo(final Object value)
	{
		Assertions.assertThat(actual.getValue()).isEqualTo(value);
		return this;
	}

	public <T extends ItemModel> AttributeValueAssert withReferenceValueEqualTo(final T referenceValue)
	{
		Assertions.assertThat(actual.getValue()).isInstanceOf(ItemPropertyValue.class);
		Assertions.assertThat(((ItemPropertyValue) actual.getValue()).getPK()).isEqualTo(referenceValue.getPk());
		return this;
	}
}
