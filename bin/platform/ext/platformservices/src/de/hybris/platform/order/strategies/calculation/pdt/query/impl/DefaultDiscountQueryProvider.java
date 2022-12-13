/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt.query.impl;

import de.hybris.platform.core.PK;
import de.hybris.platform.europe1.constants.Europe1Constants;
import de.hybris.platform.europe1.jalo.PDTRowsQueryBuilder;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.DiscountValueInfoCriteria;
import de.hybris.platform.order.strategies.calculation.pdt.impl.PDTEnumGroupsHelper;
import de.hybris.platform.order.strategies.calculation.pdt.query.PDTQueryProvider;

import org.springframework.beans.factory.annotation.Required;


public class DefaultDiscountQueryProvider implements PDTQueryProvider<DiscountValueInfoCriteria>
{
	private PDTEnumGroupsHelper pdtEnumGroupsHelper;

	@Override
	public PDTRowsQueryBuilder.QueryWithParams query(final DiscountValueInfoCriteria criteria)
	{
		final boolean global = criteria.getProduct() == null && criteria.getProductGroup() == null;
		final String discountRowTypeCode = global ? Europe1Constants.TC.GLOBALDISCOUNTROW : Europe1Constants.TC.DISCOUNTROW;

		final PK productPk = criteria.getProduct() == null ? null : criteria.getProduct().getPk();
		final PK productGroupPk = criteria.getProductGroup() == null ? null : pdtEnumGroupsHelper.getPkFromEnum(
				criteria.getProductGroup());
		final PK userPk = criteria.getUser() == null ? null : criteria.getUser().getPk();
		final PK userGroupPk = criteria.getUserGroup() == null ? null : pdtEnumGroupsHelper.getPkFromEnum(
				criteria.getUserGroup());
		final String productId = criteria.getProduct() == null ? null : criteria.getProduct().getCode();

		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(discountRowTypeCode);
		return builder.withAnyProduct().withAnyUser().withProduct(productPk).withProductId(productId)
		              .withProductGroup(productGroupPk).withUser(userPk).withUserGroup(userGroupPk).build();
	}

	@Required
	public void setPdtEnumGroupsHelper(final PDTEnumGroupsHelper pdtEnumGroupsHelper)
	{
		this.pdtEnumGroupsHelper = pdtEnumGroupsHelper;
	}
}
