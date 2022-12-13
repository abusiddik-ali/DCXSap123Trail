/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.regioncache.test;

import de.hybris.bootstrap.annotations.ManualTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.util.Collections;
import java.util.Date;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.annotation.Resource;

import org.junit.Test;

@ManualTest
public class CleanUpQueryCacheRegionPerformanceTest extends ServicelayerBaseTest
{
	@Resource
	private FlexibleSearchService flexibleSearchService;

	final AtomicBoolean sync = new AtomicBoolean(true);

	Runnable createTitleRunnable = new Runnable()
	{
		public boolean c = true;

		@Override
		public void run()
		{
			Registry.activateStandaloneMode();
			Registry.activateMasterTenant();
			final ModelService modelService = Registry.getApplicationContext().getBean("modelService", ModelService.class);
			while (sync.get())
			{
				createNewTitle(modelService);
			}
		}
	};

	Runnable createCartAndUserRunnable = new Runnable()
	{
		@Override
		public void run()
		{
			Registry.activateStandaloneMode();
			Registry.activateMasterTenant();
			final ModelService modelService = Registry.getApplicationContext().getBean("modelService", ModelService.class);
			final CommonI18NService commonI18NService = Registry.getApplicationContext()
			                                                    .getBean("commonI18NService", CommonI18NService.class);
			while (sync.get())
			{
				createNewCartAndUser(modelService, commonI18NService);
			}
		}
	};

	Runnable searchTitleRunnable = new Runnable()
	{
		@Override
		public void run()
		{
			Registry.activateStandaloneMode();
			Registry.activateMasterTenant();
			final FlexibleSearchService flexibleSearchService = Registry.getApplicationContext()
			                                                            .getBean("flexibleSearchService",
					                                                            FlexibleSearchService.class);
			while (sync.get())
			{
				selectAllTitles(flexibleSearchService);
			}
		}
	};

	Runnable searchUserRunnable = new Runnable()
	{
		@Override
		public void run()
		{
			Registry.activateStandaloneMode();
			Registry.activateMasterTenant();
			final FlexibleSearchService flexibleSearchService = Registry.getApplicationContext()
			                                                            .getBean("flexibleSearchService",
					                                                            FlexibleSearchService.class);
			while (sync.get())
			{
				selectAllUsers(flexibleSearchService);
			}
		}
	};

	Runnable searchCartRunnable = new Runnable()
	{
		public boolean c = true;

		@Override
		public void run()
		{
			Registry.activateStandaloneMode();
			Registry.activateMasterTenant();
			final FlexibleSearchService flexibleSearchService = Registry.getApplicationContext()
			                                                            .getBean("flexibleSearchService",
					                                                            FlexibleSearchService.class);
			while (sync.get())
			{
				selectAllCarts(flexibleSearchService);
			}
		}
	};

	@Test
	public void performanceTest() throws InterruptedException
	{
		fillCache();

		Thread title = new Thread(createTitleRunnable);
		title.start();

		Thread cart = new Thread(createCartAndUserRunnable);
		cart.start();

		Thread searchTitle = new Thread(searchTitleRunnable);
		searchTitle.start();

		Thread searchUser = new Thread(searchUserRunnable);
		searchUser.start();

		Thread searchCart = new Thread(searchCartRunnable);
		searchCart.start();

		Thread.sleep(1000 * 60 * 5);

		sync.set(false);
	}

	private void fillCache()
	{
		for (int i = 0; i < 5_000; i++)
		{
			final FlexibleSearchQuery query = new FlexibleSearchQuery("Select {pk} from {Product} WHERE {pk} >" + i);
			query.setResultClassList(Collections.singletonList(ProductModel.class));
			flexibleSearchService.search(query);
		}
	}

	private void selectAllTitles(FlexibleSearchService flexibleSearchService)
	{
		final FlexibleSearchQuery query = new FlexibleSearchQuery("Select {pk} from {Title}");
		query.setResultClassList(Collections.singletonList(TitleModel.class));
		final SearchResult<Object> searchResult = flexibleSearchService.search(query);
	}

	private void selectAllUsers(FlexibleSearchService flexibleSearchService)
	{
		final FlexibleSearchQuery query = new FlexibleSearchQuery("Select {pk} from {User}");
		query.setResultClassList(Collections.singletonList(UserModel.class));
		final SearchResult<Object> searchResult = flexibleSearchService.search(query);
	}

	private void selectAllCarts(FlexibleSearchService flexibleSearchService)
	{
		final FlexibleSearchQuery query = new FlexibleSearchQuery("Select {pk} from {Order}");
		query.setResultClassList(Collections.singletonList(OrderModel.class));
		final SearchResult<Object> searchResult = flexibleSearchService.search(query);
	}

	private void createNewTitle(ModelService modelService)
	{
		final TitleModel title = modelService.create(TitleModel.class);
		title.setCode(UUID.randomUUID().toString());
		modelService.save(title);
	}

	private void createNewCartAndUser(ModelService modelService, CommonI18NService commonI18NService)
	{
		final UserModel user = modelService.create(UserModel.class);
		user.setUid(UUID.randomUUID().toString());

		final OrderModel cart = modelService.create(OrderModel.class);
		cart.setUser(user);
		cart.setDate(new Date());
		cart.setCurrency(commonI18NService.getBaseCurrency());
		modelService.saveAll(user, cart);
	}
}