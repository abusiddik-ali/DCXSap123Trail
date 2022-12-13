/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.Tenant;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.UnitModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.product.UnitService;
import de.hybris.platform.servicelayer.ServicelayerTest;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;

import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class OrderConcurrencyTest extends ServicelayerTest
{
	private static final int NUMBER_OF_THREADS = 12;
	private static final int NUMBER_OF_ENTRIES = 100;

	@Resource
	CommonI18NService commonI18NService;
	@Resource
	ModelService modelService;
	@Resource
	UserService userService;
	@Resource
	ProductService productService;

	@Before
	public void setUp() throws Exception
	{
		createCoreData();
		createDefaultCatalog();
	}

	@Test
	public void shouldCreateAllEntriesConcurrently() throws InterruptedException, ExecutionException, TimeoutException
	{
		final ExecutorService es = Executors.newFixedThreadPool(NUMBER_OF_THREADS);

		PK orderPK = createOrder();
		CountDownLatch startLatch = new CountDownLatch(1);

		final List<Future<?>> creations = IntStream.range(0, NUMBER_OF_ENTRIES)
		                                           .mapToObj(i ->
				                                           new CreateEntry(
						                                           Registry.getCurrentTenant(),
						                                           orderPK, i, "pieces",
						                                           "testProduct0",
						                                           startLatch))
		                                           .map(es::submit)
		                                           .collect(Collectors.toList());

		startLatch.countDown();

		try
		{
			for (Future<?> c : creations)
			{
				final Object result = c.get(1, TimeUnit.MINUTES);
				assertThat(result).isNull();
			}
		}
		finally
		{
			es.shutdown();
			es.awaitTermination(1, TimeUnit.MINUTES);
		}

		final OrderModel order = modelService.get(orderPK);

		assertThat(order).isNotNull();
		assertThat(order.getEntries()).isNotNull().isNotEmpty().hasSize(NUMBER_OF_ENTRIES);
	}

	private PK createOrder()
	{
		modelService.detachAll();

		final CurrencyModel curr = commonI18NService.getBaseCurrency();

		final UserModel user = userService.getCurrentUser();
		final OrderModel testOrder = modelService.create(OrderModel.class);

		testOrder.setCode("TEST-" + UUID.randomUUID());
		testOrder.setUser(user);
		testOrder.setCurrency(curr);
		testOrder.setDate(new Date());
		testOrder.setNet(Boolean.FALSE);
		testOrder.setCalculated(true);

		modelService.save(testOrder);

		modelService.detachAll();

		return testOrder.getPk();
	}

	private static class CreateEntry implements Runnable
	{
		private final Tenant tenant;
		private final PK orderPk;
		private final int entryNumber;
		private final String unitCode;
		private final String productCode;
		private final CountDownLatch startLatch;

		@Resource
		ModelService modelService;
		@Resource
		ProductService productService;
		@Resource
		UnitService unitService;


		private CreateEntry(final Tenant tenant, final PK orderPk, final int entryNumber, final String unitCode,
		                    final String productCode,
		                    final CountDownLatch startLatch)
		{
			this.tenant = tenant;
			this.orderPk = orderPk;
			this.entryNumber = entryNumber;
			this.unitCode = unitCode;
			this.productCode = productCode;
			this.startLatch = startLatch;
		}

		@Override
		public void run()
		{
			try
			{
				Registry.runAsTenant(tenant, this::createOrderEntry);
			}
			catch (RuntimeException e)
			{
				throw e;
			}
			catch (InterruptedException e)
			{
				Thread.currentThread().interrupt();
			}
			catch (Exception e)
			{
				throw new RuntimeException(e);
			}
		}

		private PK createOrderEntry() throws InterruptedException
		{
			Registry.getApplicationContext().getAutowireCapableBeanFactory().autowireBean(this);

			modelService.detachAll();

			startLatch.await();

			final UnitModel unit = unitService.getUnitForCode(unitCode);
			final OrderModel order = modelService.get(orderPk);

			final OrderEntryModel orderEntry = modelService.create(OrderEntryModel.class);
			orderEntry.setProduct(productService.getProductForCode(productCode));
			orderEntry.setQuantity(1L);
			orderEntry.setUnit(unit);
			orderEntry.setBasePrice(10.0);
			orderEntry.setEntryNumber(entryNumber);
			orderEntry.setOrder(order);

			modelService.save(orderEntry);

			return orderEntry.getPk();
		}
	}
}