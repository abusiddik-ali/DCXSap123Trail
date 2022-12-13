/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.model;


import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.user.AddressModel;

import org.junit.Test;


@UnitTest
public class ItemModelNullDecoratorUnitTest
{

	@Test
	public void shouldTranslateDefaultNullToDefinedValueInGetter()
	{
		// assuming that Address.duplicate is having a <nulldecorator>Boolean.FALSE</nulldecorator> in place:
		// given
		final AddressModel address = new AddressModel();

		// then
		assertThat(address.getDuplicate()).isEqualTo(Boolean.FALSE);
	}

	@Test
	public void shouldTranslateNullSetterToDefinedValueInGetter()
	{
		// assuming that Address.duplicate is having a <nulldecorator>Boolean.FALSE</nulldecorator> in place:
		// given
		final AddressModel address = new AddressModel();

		// when
		address.setDuplicate(null);

		// then
		assertThat(address.getDuplicate()).isEqualTo(Boolean.FALSE);
	}

}
