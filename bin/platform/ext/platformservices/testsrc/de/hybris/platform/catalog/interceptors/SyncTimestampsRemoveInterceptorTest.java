/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.interceptors;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.catalog.daos.ItemSyncTimestampDao;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.ItemSyncTimestampModel;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PersistenceOperation;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.util.config.PropertyActionReader;

import java.util.Arrays;

import org.apache.commons.configuration.Configuration;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;


@UnitTest
public class SyncTimestampsRemoveInterceptorTest
{
	SyncTimestampsRemoveInterceptor remSyncTimestampsInt;

	@Mock
	private ItemSyncTimestampDao itemSyncTimestampDao;

	@Mock
	private ModelService modelService;

	@Mock
	private ConfigurationService configurationService;

	@Mock
	private Configuration configuration;

	@Mock
	private PropertyActionReader propertyActionReader;

	@Mock
	private InterceptorContext interceptorContext;

	@Before
	public void setup()
	{
		MockitoAnnotations.initMocks(this);
		Mockito.when(interceptorContext.getModelService()).thenReturn(modelService);
		Mockito.when(configurationService.getConfiguration()).thenReturn(configuration);

		remSyncTimestampsInt = new SyncTimestampsRemoveInterceptor();
		remSyncTimestampsInt.setItemSyncTimestampDao(itemSyncTimestampDao);
		remSyncTimestampsInt.setConfigurationService(configurationService);
		remSyncTimestampsInt.setPropertyActionReader(propertyActionReader);
	}


	@Test
	public void testRemove() throws InterceptorException
	{
		final ProductModel product = new ProductModel();
		final Integer limit = Integer.valueOf(2);
		final ItemSyncTimestampModel itemSyncTimestampModel1 = new ItemSyncTimestampModel();
		final ItemSyncTimestampModel itemSyncTimestampModel2 = new ItemSyncTimestampModel();
		final ItemSyncTimestampModel itemSyncTimestampModel3 = new ItemSyncTimestampModel();
		final ItemSyncTimestampModel itemSyncTimestampModel4 = new ItemSyncTimestampModel();

		remSyncTimestampsInt.setLimit(limit);

		Mockito.when(itemSyncTimestampDao.findSyncTimestampsByItem(product, limit.intValue())).thenReturn(
				Arrays.asList(itemSyncTimestampModel1, itemSyncTimestampModel2, itemSyncTimestampModel3,
						itemSyncTimestampModel4));

		Mockito.when(modelService.getModelType(product)).thenReturn("Product");
		Mockito.when(modelService.getModelType(itemSyncTimestampModel1)).thenReturn("ItemSyncTimestampModel");

		remSyncTimestampsInt.onRemove(product, interceptorContext);

		Mockito.verify(interceptorContext).registerElementFor(itemSyncTimestampModel1, PersistenceOperation.DELETE);
		Mockito.verify(interceptorContext).registerElementFor(itemSyncTimestampModel2, PersistenceOperation.DELETE);
		Mockito.verify(interceptorContext).registerElementFor(itemSyncTimestampModel3, PersistenceOperation.DELETE);
		Mockito.verify(interceptorContext).registerElementFor(itemSyncTimestampModel4, PersistenceOperation.DELETE);
	}

	@Test
	public void testCannotRemove() throws InterceptorException
	{
		final CatalogModel catalogModel = new CatalogModel();
		Mockito.when(modelService.getModelType(catalogModel)).thenReturn("Catalog");
		Mockito.when(propertyActionReader.isActionDisabledForType("synctimestamp.removal", "Catalog")).thenReturn(true);

		remSyncTimestampsInt.onRemove(catalogModel, interceptorContext);

		Mockito.verify(itemSyncTimestampDao, Mockito.times(0)).findSyncTimestampsByItem((ItemModel) Mockito.anyObject(),
				Mockito.anyInt());
	}
}
