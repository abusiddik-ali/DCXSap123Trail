/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.ordercloning;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.order.AbstractOrderEntryTypeService;
import de.hybris.platform.order.CartService;
import de.hybris.platform.order.strategies.ordercloning.impl.DefaultCloneAbstractOrderStrategy;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.ServicelayerTransactionalTest;
import de.hybris.platform.servicelayer.internal.model.impl.ItemModelCloneCreator;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.type.TypeService;

import java.util.Collections;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class CloneOrderStrategyAdjustEntryNumbersHookTest extends ServicelayerTransactionalTest
{

	private DefaultCloneAbstractOrderStrategy cloneAbstractOrderStrategy;
	@Resource
	private CartService cartService;
	@Resource
	private ProductService productService;
	@Resource
	private TypeService typeService;
	@Resource
	private ModelService modelService;
	@Resource
	private AbstractOrderEntryTypeService abstractOrderEntryTypeService;
	@Resource
	private ItemModelCloneCreator itemModelCloneCreator;


	@Before
	public void setUp() throws Exception
	{
		cloneAbstractOrderStrategy = new DefaultCloneAbstractOrderStrategy(typeService, itemModelCloneCreator,
				abstractOrderEntryTypeService);

		createCoreData();
		createDefaultCatalog();
	}

	@Test
	public void testCloneWithAdjustEntryHook()
	{
		final CartModel cart = createCartWith3Products();

		final ComposedTypeModel orderType = typeService.getComposedTypeForCode("Order");
		final ComposedTypeModel orderEntryType = typeService.getComposedTypeForCode("OrderEntry");

		associateStrategyWithTestHook(cloneAbstractOrderStrategy);

		final OrderModel order = cloneAbstractOrderStrategy.clone(orderType, orderEntryType, cart,
				"orderCode_" + cart.getCode(),
				OrderModel.class, OrderEntryModel.class);

		assertThat(cart.getEntries()).extracting(AbstractOrderEntryModel::getEntryNumber).containsExactly(0, 1, 2);
		assertThat(order.getEntries()).extracting(AbstractOrderEntryModel::getEntryNumber).containsExactly(0, 10, 20);
	}

	private void associateStrategyWithTestHook(final DefaultCloneAbstractOrderStrategy strategy)
	{
		strategy.setCloneHooks(Collections.singletonList(new AdjustEntryTestHook()));
	}

	private CartModel createCartWith3Products()
	{
		final ProductModel product1 = productService.getProductForCode("testProduct1");
		final ProductModel product2 = productService.getProductForCode("testProduct2");
		final ProductModel product3 = productService.getProductForCode("testProduct3");
		modelService.saveAll();

		final CartModel cart = cartService.getSessionCart();
		cartService.addNewEntry(cart, product1, 3, null);
		cartService.addNewEntry(cart, product2, 2, null);
		cartService.addNewEntry(cart, product3, 4, null);
		modelService.save(cart);
		return cart;
	}
}
