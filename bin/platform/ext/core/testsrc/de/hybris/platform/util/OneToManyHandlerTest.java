/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.jalo.Item;
import de.hybris.platform.jalo.SessionContext;
import de.hybris.platform.jalo.product.Product;
import de.hybris.platform.jalo.type.CollectionType;
import de.hybris.platform.testframework.HybrisJUnit4Test;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import org.junit.After;
import org.junit.Test;
import org.mockito.Mockito;

@UnitTest
public class OneToManyHandlerTest extends HybrisJUnit4Test
{

	private static final String POSITION = "featurePosition";
	private static final int CALCULATED_POSITION = 2;
	private static final PropertyConfigSwitcher product2FeatureRelation = new PropertyConfigSwitcher(
			"relation.Product2FeatureRelation.reordered");


	OneToManyHandler handler = new BidirectionalOneToManyHandler(
			"ProductFeature",
			false,
			"product",
			POSITION,
			true,
			true,
			CollectionType.LIST
	).withRelationCode("Product2FeatureRelation");

	OneToManyHandler handlerWithNoRelationCode = new BidirectionalOneToManyHandler(
			"ProductFeature",
			false,
			"product",
			POSITION,
			true,
			true,
			CollectionType.LIST
	);

	@After
	public void after()
	{
		product2FeatureRelation.switchToValue("false");
	}

	@Test
	public void shouldGetPositionFromGetNextOrderNumberQuery()
	{
		//given
		product2FeatureRelation.switchToValue("false");
		final OneToManyHandler spiedHandler = Mockito.spy(handler);
		final Item.ItemAttributeMap attributes = new Item.ItemAttributeMap();

		final Product product = new Product();
		attributes.put("product", product);

		final SessionContext ctx = new SessionContext();

		Mockito.doReturn(CALCULATED_POSITION).when(spiedHandler).getNextOrderNumber(ctx, product);

		//when
		spiedHandler.newInstance(ctx, attributes);

		//then
		Mockito.verify(spiedHandler).getNextOrderNumber(ctx, product);
		assertThat(attributes.get(POSITION)).isEqualTo(CALCULATED_POSITION);
	}

	@Test
	public void shouldLeavePositionNullWhenPropertySetToAlways()
	{
		//given
		product2FeatureRelation.switchToValue("always");

		final Item.ItemAttributeMap attributes = new Item.ItemAttributeMap();

		final Product product = new Product();
		attributes.put("product", product);

		final SessionContext ctx = new SessionContext();

		//when
		handler.newInstance(ctx, attributes);

		//then
		assertThat(attributes.get(POSITION)).isEqualTo(null);
	}

	@Test
	public void shouldGetPositionFromGetNextOrderNumberQueryWhenPropertySetToSync()
	{
		//given
		product2FeatureRelation.switchToValue("sync");
		final OneToManyHandler spiedHandler = Mockito.spy(handler);
		final Item.ItemAttributeMap attributes = new Item.ItemAttributeMap();

		final Product product = new Product();
		attributes.put("product", product);

		final SessionContext ctx = new SessionContext();

		Mockito.doReturn(CALCULATED_POSITION).when(spiedHandler).getNextOrderNumber(ctx, product);

		//when
		spiedHandler.newInstance(ctx, attributes);

		//then
		Mockito.verify(spiedHandler).getNextOrderNumber(ctx, product);
		assertThat(attributes.get(POSITION)).isEqualTo(CALCULATED_POSITION);
	}

	@Test
	public void shouldLeavePositionNullWhenPropertySetToSyncAndInSync()
	{
		//given
		product2FeatureRelation.switchToValue("sync");

		final Item.ItemAttributeMap attributes = new Item.ItemAttributeMap();

		final Product product = new Product();
		attributes.put("product", product);

		final SessionContext ctx = new SessionContext();
		ctx.setAttribute("catalog.sync.active", true);

		//when
		handler.newInstance(ctx, attributes);

		//then
		assertThat(attributes.get(POSITION)).isEqualTo(null);
	}

	@Test
	public void shouldLeavePositionNullWhenPropertySetToAlwaysWithNoRelationCode()
	{
		//given
		product2FeatureRelation.switchToValue("always");

		final Item.ItemAttributeMap attributes = new Item.ItemAttributeMap();

		final Product product = new Product();
		attributes.put("product", product);

		final SessionContext ctx = new SessionContext();

		//when
		handlerWithNoRelationCode.newInstance(ctx, attributes);

		//then
		assertThat(attributes.get(POSITION)).isEqualTo(null);
	}

	@Test
	public void shouldLeavePositionNullWhenPropertySetToSyncAndInSyncWithNoRelationCode()
	{
		//given
		product2FeatureRelation.switchToValue("sync");

		final Item.ItemAttributeMap attributes = new Item.ItemAttributeMap();

		final Product product = new Product();
		attributes.put("product", product);

		final SessionContext ctx = new SessionContext();
		ctx.setAttribute("catalog.sync.active", true);

		//when
		handlerWithNoRelationCode.newInstance(ctx, attributes);

		//then
		assertThat(attributes.get(POSITION)).isEqualTo(null);
	}
}