/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.product.impl;


import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.product.ConfiguratorSettingsService;
import de.hybris.platform.product.ConfiguratorSettingsResolutionStrategy;
import de.hybris.platform.product.model.AbstractConfiguratorSettingModel;

import org.springframework.beans.factory.annotation.Required;

import javax.annotation.Nonnull;

import java.util.List;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


/**
 * Default implementation of {@code ConfiguratorSettingsService}.
 */
public class DefaultConfiguratorSettingsService implements ConfiguratorSettingsService
{
	private ConfiguratorSettingsResolutionStrategy configuratorSettingsResolutionStrategy;

	@Nonnull
	@Override
	public List<AbstractConfiguratorSettingModel> getConfiguratorSettingsForProduct(
			@Nonnull final ProductModel product)
	{
		validateParameterNotNullStandardMessage("product", product);
		return getConfiguratorSettingsResolutionStrategy().getConfiguratorSettingsForProduct(product);
	}

	protected ConfiguratorSettingsResolutionStrategy getConfiguratorSettingsResolutionStrategy()
	{
		return configuratorSettingsResolutionStrategy;
	}

	@Required
	public void setConfiguratorSettingsResolutionStrategy(
			final ConfiguratorSettingsResolutionStrategy configuratorSettingsResolutionStrategy)
	{
		this.configuratorSettingsResolutionStrategy = configuratorSettingsResolutionStrategy;
	}
}
