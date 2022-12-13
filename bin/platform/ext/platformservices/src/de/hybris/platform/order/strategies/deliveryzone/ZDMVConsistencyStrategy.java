/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.deliveryzone;

import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.deliveryzone.model.ZoneModel;

import java.util.Map;
import java.util.Set;


/**
 * Strategy for consistency check for {@link ZoneDeliveryModeModel}.
 *
 * @spring.bean zdmvConsistencyStrategy
 */
public interface ZDMVConsistencyStrategy
{

	/**
	 * Gets all {@link CountryModel}s and its corresponding {@link ZoneModel}s when the {@link CountryModel} belongs to
	 * more than one {@link ZoneModel}.
	 *
	 * @param zones the zones
	 * @return a <code>Map</code> which contains {@link CountryModel} and its {@link ZoneModel}s, when the
	 * {@link CountryModel} belongs to more than one {@link ZoneModel}. <b>Collections.EMPTY_MAP</b> otherwise.
	 */
	Map<CountryModel, Set<ZoneModel>> getAmbiguousCountriesForZones(final Set<ZoneModel> zones);

}
