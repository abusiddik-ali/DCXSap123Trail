/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.model;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.ServicelayerTest;
import de.hybris.platform.servicelayer.internal.converter.ConverterRegistry;
import de.hybris.platform.servicelayer.type.TypeService;
import de.hybris.platform.tx.Transaction;

import javax.annotation.Resource;

import org.junit.Test;

@IntegrationTest
public class RefreshModelConverterAdditionalTest extends ServicelayerTest
{
	@Resource
	private ConverterRegistry converterRegistry;
	@Resource
	private ModelService modelService;
	@Resource
	private TypeService typeService;

	@Test
	public void shouldRefreshConvertersAfterTransactionRollback()
	{
		final RefreshModelConverterTestUtils util = new RefreshModelConverterTestUtils(converterRegistry, modelService,
				typeService);

		final Transaction tx = Transaction.current();
		tx.begin();

		final String qualifier = util.generateQualifier();
		util.createAttribute(UserModel.class, qualifier);
		util.checkConverterForAttribute("user", qualifier, true);
		util.checkConverterForAttribute("employee", qualifier, true);

		tx.rollback();

		util.checkConverterForAttribute("user", qualifier, false);
		util.checkConverterForAttribute("employee", qualifier, false);

	}


}
