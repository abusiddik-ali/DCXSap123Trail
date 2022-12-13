/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.product.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.core.model.product.UnitModel;
import de.hybris.platform.servicelayer.exceptions.AmbiguousIdentifierException;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.internal.dao.AbstractItemDao;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.util.List;


/**
 * @deprecated since ages - as of release 4.3, please use{@link de.hybris.platform.product.daos.impl.DefaultUnitDao}
 * <p>
 * Default implementation of the {@link UnitDao}.
 */
@Deprecated(since = "ages", forRemoval = true)
@SuppressWarnings("deprecation")
public class DefaultUnitDao extends AbstractItemDao implements UnitDao
{

	@Override
	public UnitModel findUnit(final String code)
	{
		validateParameterNotNull(code, "Code must not be null!");
		final FlexibleSearchQuery query = new FlexibleSearchQuery( //
				"SELECT {" + UnitModel.PK + "} " + //
						"FROM {" + UnitModel._TYPECODE + "} " + //
						"WHERE {" + UnitModel.CODE + "}=?code");

		query.addQueryParameter(UnitModel.CODE, code);
		final SearchResult<UnitModel> result = getFlexibleSearchService().search(query);
		final List<UnitModel> units = result.getResult();

		if (units.isEmpty())
		{
			throw new UnknownIdentifierException("Unit with code '" + code + "' not found!");
		}
		else if (units.size() > 1)
		{
			throw new AmbiguousIdentifierException("Code '" + code + "' is not unique, " + units.size() + " units found!");
		}
		return units.get(0);
	}

}
