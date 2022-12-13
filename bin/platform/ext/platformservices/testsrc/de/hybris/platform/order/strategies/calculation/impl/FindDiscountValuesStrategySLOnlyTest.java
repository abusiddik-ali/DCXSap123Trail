/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.price.DiscountModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.product.UnitModel;
import de.hybris.platform.europe1.model.DiscountRowModel;
import de.hybris.platform.jalo.order.price.DiscountInformation;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.order.strategies.calculation.FindDiscountValuesHook;
import de.hybris.platform.order.strategies.calculation.impl.servicelayer.DefaultSLFindDiscountValuesStrategy;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.impl.DefaultDiscountValueInfoCriteria;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.util.DiscountValue;

import java.util.Date;
import java.util.List;
import java.util.UUID;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class FindDiscountValuesStrategySLOnlyTest extends ServicelayerBaseTest
{

	@Resource
	private DefaultSLFindDiscountValuesStrategy slFindDiscountValuesStrategy;

	@Resource
	private ModelService modelService;

	@Resource
	private SessionService sessionService;

	private List<FindDiscountValuesHook> cachedHooks;

	@Before
	public void setUp() throws Exception
	{
		getOrCreateLanguage("de");
		cachedHooks = slFindDiscountValuesStrategy.getFindDiscountValuesHooks();
		slFindDiscountValuesStrategy.setFindDiscountValuesHooks(List.of(new TestFindDiscountValuesHook()));
	}

	@After
	public void tearDown() throws Exception
	{
		slFindDiscountValuesStrategy.setFindDiscountValuesHooks(cachedHooks);
	}

	@Test
	public void shouldReturnDiscountInformations() throws CalculationException
	{
		// given
		final CatalogModel catalog = modelService.create(CatalogModel.class);
		catalog.setId(UUID.randomUUID().toString());

		final CatalogVersionModel catalogVersion = modelService.create(CatalogVersionModel.class);
		catalogVersion.setCatalog(catalog);
		catalogVersion.setVersion(UUID.randomUUID().toString());

		final ProductModel product = modelService.create(ProductModel.class);
		product.setCode(UUID.randomUUID().toString());
		product.setCatalogVersion(catalogVersion);

		final DiscountModel discount = modelService.create(DiscountModel.class);
		discount.setCode(UUID.randomUUID().toString());
		discount.setValue(10.0);

		final DiscountRowModel discountRow = modelService.create(DiscountRowModel.class);
		discountRow.setProduct(product);
		discountRow.setDiscount(discount);

		modelService.saveAll();

		// when
		final List<DiscountInformation> discountInformations = slFindDiscountValuesStrategy
				.getDiscountInformation(DefaultDiscountValueInfoCriteria.buildForInfo() //
				                                                        .withProduct(product) //
				                                                        .withDate(new Date()) //
				                                                        .withUser(sessionService.getCurrentSession()
				                                                                                .getAttribute("user")) //
				                                                        .withCurrency(sessionService.getCurrentSession()
				                                                                                    .getAttribute("currency"))
				                                                        .build());

		// then
		assertThat(discountInformations).hasSize(1);

		final DiscountInformation discountInfo = discountInformations.get(0);
		assertThat(discountInfo.getValue().getCode()).isEqualTo(discount.getCode());
		assertThat(discountInfo.getValue().getValue()).isEqualTo(10.0);
	}

	@Test
	public void shouldReturnDiscountValuesWithHook() throws CalculationException
	{
		// given
		final CatalogModel catalog = modelService.create(CatalogModel.class);
		catalog.setId(UUID.randomUUID().toString());

		final CatalogVersionModel catalogVersion = modelService.create(CatalogVersionModel.class);
		catalogVersion.setCatalog(catalog);
		catalogVersion.setVersion(UUID.randomUUID().toString());

		final ProductModel product = modelService.create(ProductModel.class);
		product.setCode(UUID.randomUUID().toString());
		product.setCatalogVersion(catalogVersion);

		final DiscountValue discount = new DiscountValue(UUID.randomUUID().toString(), 10.0, true, "USD");

		final OrderModel order = modelService.create(OrderModel.class);
		order.setUser(sessionService.getCurrentSession().getAttribute("user"));
		order.setCurrency(sessionService.getCurrentSession().getAttribute("currency"));
		order.setDate(new Date());

		final OrderEntryModel orderEntryModel = modelService.create(OrderEntryModel.class);
		orderEntryModel.setProduct(product);
		orderEntryModel.setDiscountValues(List.of(discount));
		orderEntryModel.setOrder(order);
		orderEntryModel.setQuantity(1L);
		UnitModel unit = modelService.create(UnitModel.class);
		unit.setCode(UUID.randomUUID().toString());
		unit.setUnitType("USD");
		orderEntryModel.setUnit(unit);

		modelService.saveAll();

		final List<DiscountValue> discountValues = slFindDiscountValuesStrategy
				.findDiscountValues(orderEntryModel);

		// then
		assertThat(discountValues).hasSize(1);

		final DiscountValue discountInfo = discountValues.get(0);
		assertThat(discountInfo.getValue()).isEqualTo(20.0);
	}

	private static class TestFindDiscountValuesHook implements FindDiscountValuesHook
	{

		@Override
		public List<DiscountValue> findDiscountValues(AbstractOrderEntryModel entry)
		{
			DiscountValue discountValue = new DiscountValue(UUID.randomUUID().toString(), 20.0, true, "USD");
			return List.of(discountValue);
		}

		@Override
		public boolean isApplicable(AbstractOrderEntryModel entry)
		{
			return true;
		}
	}
}