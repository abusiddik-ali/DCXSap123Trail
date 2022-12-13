/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jalo.enumeration;


import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.enumeration.EnumerationValueModel;
import de.hybris.platform.europe1.enums.ProductPriceGroup;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;

import java.util.UUID;

import javax.annotation.Resource;

import org.junit.Test;

@IntegrationTest
public class EnumerationValueOwnerTest extends ServicelayerBaseTest
{
	@Resource
	ModelService modelService;
	@Resource
	UserService userService;

	@Test
	public void shouldSetOwnerToTheGenericItem()
	{
		final EnumerationValueModel enumValue = modelService.create(ProductPriceGroup._TYPECODE);

		enumValue.setCode("CODE_" + UUID.randomUUID().toString());
		enumValue.setOwner(userService.getCurrentUser());

		modelService.saveAll();
		modelService.refresh(enumValue);

		assertThat(enumValue.getOwner()).isNotNull();
	}
}