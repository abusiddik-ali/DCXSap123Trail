/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jalo;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.enums.GroupType;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.order.EntryGroup;
import de.hybris.platform.jalo.order.Cart;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.user.UserService;

import org.junit.Test;

import javax.annotation.Resource;

import java.util.Date;
import java.util.List;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

@IntegrationTest
public class HybrisEnumValueSerializationTest extends ServicelayerBaseTest
{
	@Resource
	private ModelService modelService;
	@Resource
	private UserService userService;
	@Resource
	private CommonI18NService commonI18NService;
	@Resource
	private FlexibleSearchService flexibleSearchService;

	public static final String ENUM_VALUE = "ENUM_VALUE";

	@Test
	public void saveAndReadCartShouldDeserializeEnumsProperly()
	{
		final String someUniqueCode = "349898323298";
		//given
		final EntryGroup entryGroup = new EntryGroup();
		final GroupType groupType = GroupType.valueOf(ENUM_VALUE);
		entryGroup.setGroupType(groupType);
		final List<EntryGroup> entryGroups = Lists.newArrayList(entryGroup);
		final CartModel cart = modelService.create(CartModel.class);
		cart.setEntryGroups(entryGroups);
		cart.setCurrency(commonI18NService.getBaseCurrency());
		cart.setDate(new Date());
		cart.setUser(userService.getCurrentUser());
		cart.setCode(someUniqueCode);

		//when
		modelService.save(cart);

		//then
		final List<CartModel> carts = flexibleSearchService
				.<CartModel>search("get {Cart} where {code}=?code", ImmutableMap.of("code", someUniqueCode))
				.getResult();

		assertThat(carts.size()).isEqualTo(1);
		final GroupType deserializedGroupType = carts.get(0).getEntryGroups().get(0).getGroupType();
		assertThat(deserializedGroupType).isEqualTo(groupType);
		assertThat(deserializedGroupType == groupType).isTrue();
	}
}
