/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.converters;

import de.hybris.platform.converters.config.ConfigurablePopulatorModification;


/**
 * Interface for modifiable configurable populators.
 */
public interface ModifiableConfigurablePopulator<SOURCE, TARGET, OPTION> extends ConfigurablePopulator<SOURCE, TARGET, OPTION>
{
	void applyModification(ConfigurablePopulatorModification<SOURCE, TARGET, OPTION> modification);
}
