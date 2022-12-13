/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.product.impl;

import de.hybris.platform.core.model.product.UnitModel;
import de.hybris.platform.servicelayer.internal.dao.Dao;


/**
 * The {@link UnitModel} DAO.
 *
 * @deprecated since ages - as of release 4.3, please use{@link de.hybris.platform.product.daos.UnitDao}
 */
@Deprecated(since = "ages", forRemoval = true)
public interface UnitDao extends Dao
{
	/**
	 * @param code the {@link UnitModel#CODE}
	 * @return for the given <code>code</code> the {@link UnitModel}
	 * @throws IllegalArgumentException                                                if <code>code</code> is <code>null</code>.
	 * @throws de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException   if no unit was found
	 * @throws de.hybris.platform.servicelayer.exceptions.AmbiguousIdentifierException if more than one unit was found by this code
	 * @deprecated since ages - as of release 4.3, please use{@link de.hybris.platform.product.daos.UnitDao#findUnitsByCode(String)}
	 */
	@Deprecated(since = "ages", forRemoval = true)
	UnitModel findUnit(final String code);
}
