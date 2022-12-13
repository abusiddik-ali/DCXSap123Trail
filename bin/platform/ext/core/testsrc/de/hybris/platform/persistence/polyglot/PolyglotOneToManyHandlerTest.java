/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.persistence.polyglot;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.hasProperty;
import static org.mockito.Matchers.argThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.Constants;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.product.UnitModel;
import de.hybris.platform.persistence.polyglot.config.RepositoryResult;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.testframework.seed.TestDataCreator;
import de.hybris.platform.util.logging.HybrisLogListener;
import de.hybris.platform.util.logging.HybrisLogger;

import java.util.Date;
import java.util.List;
import java.util.UUID;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

@IntegrationTest
public class PolyglotOneToManyHandlerTest extends ServicelayerBaseTest
{
	@Resource
	private ModelService modelService;
	@Resource
	private UserService userService;
	@Resource
	private CommonI18NService commonI18NService;

	private TestDataCreator dataCreator;
	private UnitModel unit;
	private CatalogVersionModel catalogVersion;
	private HybrisLogListener listener;

	@Before
	public void setUp() throws Exception
	{
		unit = createUnit();

		dataCreator = new TestDataCreator(modelService);

		final CatalogModel catalog = dataCreator.createCatalog();
		catalogVersion = dataCreator.createCatalogVersion("1.0", catalog);


		listener = mock(HybrisLogListener.class);
		when(listener.isEnabledFor(Mockito.any())).thenReturn(true);

		HybrisLogger.addListener(listener);
	}

	@After
	public void tearDown() throws Exception
	{
		HybrisLogger.removeListener(listener);
	}

	@Test
	public void shouldUsePolyglotQueryForTypesSupportedByPolyglot()
	{
		final RepositoryResult repository = PolyglotPersistence.getRepository(TypeInfoFactory.getTypeInfo(Constants.TC.Cart));
		Assume.assumeFalse(repository.isFullySupported());

		final CartModel cart = createCart("polyglot expected test cart", 0.0);

		createCartEntry(unit, dataCreator.createProduct(catalogVersion), cart, 2L);
		createCartEntry(unit, dataCreator.createProduct(catalogVersion), cart, 3L);

		modelService.saveAll();

		final List<AbstractOrderEntryModel> entries = cart.getEntries();

		assertThat(entries).hasSize(2);
		verify(listener, never()).log(argThat(hasProperty("renderedMessage", containsString("EXPECTED POLYGLOT"))));
	}

	private CartModel createCart(final String cartDescription, final Double paymentCost)
	{
		final CartModel cartModel = modelService.create(CartModel.class);
		cartModel.setUser(userService.getAdminUser());
		final Date date = new Date();
		cartModel.setDate(date);
		cartModel.setCurrency(commonI18NService.getBaseCurrency());
		cartModel.setDescription(cartDescription);
		cartModel.setPaymentCost(paymentCost);
		final String uuid = UUID.randomUUID().toString();
		cartModel.setCode(uuid);
		return cartModel;
	}

	private CartEntryModel createCartEntry(final UnitModel unit, final ProductModel product, final CartModel order,
	                                      final Long quantity)
	{
		final CartEntryModel cartEntryModel = modelService.create(CartEntryModel.class);
		cartEntryModel.setOrder(order);
		cartEntryModel.setQuantity(quantity);
		cartEntryModel.setUnit(unit);
		cartEntryModel.setProduct(product);

		return cartEntryModel;
	}

	private UnitModel createUnit()
	{
		final UnitModel unit = modelService.create(UnitModel.class);
		unit.setName("pieces");
		unit.setUnitType("type");
		unit.setCode(UUID.randomUUID().toString());
		return unit;
	}
}
