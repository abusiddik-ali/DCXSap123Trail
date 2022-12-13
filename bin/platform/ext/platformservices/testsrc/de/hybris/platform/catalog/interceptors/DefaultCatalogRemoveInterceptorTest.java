/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.interceptors;

import static org.assertj.core.api.Assertions.assertThatThrownBy;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.servicelayer.i18n.L10NService;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;


@UnitTest
public class DefaultCatalogRemoveInterceptorTest
{
	private DefaultCatalogRemoveInterceptor defaultCatalogRemoveCatalogRemoveInterceptor;

	@Mock
	private L10NService l10nService;

	private CatalogModel catalog;

	@Before
	public void setUp()
	{
		MockitoAnnotations.initMocks(this);

		defaultCatalogRemoveCatalogRemoveInterceptor = new DefaultCatalogRemoveInterceptor();
		defaultCatalogRemoveCatalogRemoveInterceptor.setL10nService(l10nService);

		catalog = new CatalogModel();

	}

	@Test
	public void revomeTest() throws InterceptorException
	{
		catalog.setDefaultCatalog(Boolean.FALSE);

		defaultCatalogRemoveCatalogRemoveInterceptor.onRemove(catalog, null);
	}

	@Test
	public void revomeDefaultTest()
	{
		catalog.setDefaultCatalog(Boolean.TRUE);
		Mockito.when(l10nService.getLocalizedString("error.catalog.removing_default_catalog")).thenReturn("Error.");

		assertThatThrownBy(() -> defaultCatalogRemoveCatalogRemoveInterceptor.onRemove(catalog, null)).isInstanceOf(
				InterceptorException.class);
	}

}
