/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt.query.impl;

import de.hybris.platform.core.PK;
import de.hybris.platform.europe1.jalo.PDTRowsQueryBuilder;
import de.hybris.platform.europe1.model.PriceRowModel;
import de.hybris.platform.jalo.c2l.Currency;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.PriceValueInfoCriteria;
import de.hybris.platform.order.strategies.calculation.pdt.impl.PDTEnumGroupsHelper;
import de.hybris.platform.order.strategies.calculation.pdt.query.PDTQueryProvider;

import java.util.Date;
import java.util.LinkedHashMap;

import org.springframework.beans.factory.annotation.Required;


public class DefaultPriceQueryProvider implements PDTQueryProvider<PriceValueInfoCriteria>
{
	private PDTEnumGroupsHelper pdtEnumGroupsHelper;

	@Override
	public PDTRowsQueryBuilder.QueryWithParams query(final PriceValueInfoCriteria criteria)
	{
		final PK productPk = criteria.getProduct() == null ? null : criteria.getProduct().getPk();
		final PK productGroupPk = criteria.getProductGroup() == null ? null
				: pdtEnumGroupsHelper.getPkFromEnum(criteria.getProductGroup());
		final PK userPk = criteria.getUser() == null ? null : criteria.getUser().getPk();
		final PK userGroupPk = criteria.getUserGroup() == null ? null : pdtEnumGroupsHelper.getPkFromEnum(
				criteria.getUserGroup());
		final String productId = criteria.getProduct() == null ? null : criteria.getProduct().getCode();

		final LinkedHashMap<String, Class> columns = new LinkedHashMap<>();

		columns.put(PriceRowModel.PK, PK.class);
		columns.put(PriceRowModel.STARTTIME, Date.class);
		columns.put(PriceRowModel.ENDTIME, Date.class);
		columns.put(PriceRowModel.CURRENCY, Currency.class);
		columns.put(PriceRowModel.GIVEAWAYPRICE, Boolean.class);

		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(PriceRowModel._TYPECODE, columns);
		return builder.withAnyProduct().withAnyUser().withProduct(productPk).withProductId(productId)
		              .withProductGroup(productGroupPk).withUser(userPk).withUserGroup(userGroupPk).build();
	}

	@Required
	public void setPdtEnumGroupsHelper(final PDTEnumGroupsHelper pdtEnumGroupsHelper)
	{
		this.pdtEnumGroupsHelper = pdtEnumGroupsHelper;
	}
}
