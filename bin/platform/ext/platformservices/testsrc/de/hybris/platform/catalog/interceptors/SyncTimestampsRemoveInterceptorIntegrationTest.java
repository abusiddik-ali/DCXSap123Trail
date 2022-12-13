/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.interceptors;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.daos.ItemSyncTimestampDao;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.config.PropertyActionReader;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;


@IntegrationTest
public class SyncTimestampsRemoveInterceptorIntegrationTest extends ServicelayerTransactionalBaseTest
{
	private final PropertyConfigSwitcher removalOptimisationSwitcher = new PropertyConfigSwitcher(
			"synctimestamp.query.removal.optimisation");

	SyncTimestampsRemoveInterceptor remSyncTimestampsInt;

	@Mock
	private ItemSyncTimestampDao itemSyncTimestampDao;

	@Resource
	private ModelService modelService;

	@Resource
	private ConfigurationService configurationService;

	@Resource
	private PropertyActionReader propertyActionReader;

	@Resource
	private SessionService sessionService;

	@Mock
	private InterceptorContext interceptorContext;

	private ProductModel product;

	@Before
	public void setup()
	{
		final CatalogModel catalog = this.modelService.create(CatalogModel.class);
		catalog.setId("my_catalog");
		modelService.save(catalog);

		final CatalogVersionModel catalogVersionModel = modelService.create(CatalogVersionModel.class);
		catalogVersionModel.setVersion("online_version");
		catalogVersionModel.setCatalog(catalog);
		modelService.save(catalogVersionModel);

		product = modelService.create(ProductModel._TYPECODE);
		product.setCode("test");
		product.setCatalogVersion(catalogVersionModel);
		modelService.save(product);

		MockitoAnnotations.initMocks(this);
		Mockito.when(interceptorContext.getModelService()).thenReturn(modelService);

		remSyncTimestampsInt = new SyncTimestampsRemoveInterceptor();
		remSyncTimestampsInt.setItemSyncTimestampDao(itemSyncTimestampDao);
		remSyncTimestampsInt.setConfigurationService(configurationService);
		remSyncTimestampsInt.setPropertyActionReader(propertyActionReader);
		remSyncTimestampsInt.setSessionService(sessionService);
	}

	@After
	public void cleanup()
	{
		removalOptimisationSwitcher.switchBackToDefault();
	}

	@Test
	public void testWithQueryOptimisationMarker() throws InterceptorException
	{
		removalOptimisationSwitcher.switchToValue("true");

		remSyncTimestampsInt.onRemove(product, interceptorContext);

		assertThat((Boolean)getAttributeFromSessionForProductPk()).isTrue();
	}

	@Test
	public void testWithoutQueryOptimisationMarker() throws InterceptorException
	{
		removalOptimisationSwitcher.switchToValue("false");

		remSyncTimestampsInt.onRemove(product, interceptorContext);

		assertThat(getAttributeFromSessionForProductPk()).isNull();
	}

	private Object getAttributeFromSessionForProductPk()
	{
		return sessionService.getAttribute("sync.timestamps.removed.for." + product.getPk());
	}
}
