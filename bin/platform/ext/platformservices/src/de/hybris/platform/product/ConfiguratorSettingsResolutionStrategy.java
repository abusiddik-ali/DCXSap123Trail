/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.product;

import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.product.model.AbstractConfiguratorSettingModel;

import java.util.List;

import javax.annotation.Nonnull;


/**
 * Collects configurator settings from categories.
 *
 * @see de.hybris.platform.category.model.ConfigurationCategoryModel
 * @see ConfiguratorSettingsService#getConfiguratorSettingsForProduct(ProductModel)
 */
public interface ConfiguratorSettingsResolutionStrategy
{
	/**
	 * Collects configurator settings for given product filtered by settings' qualifier.
	 * To override a setting you should create one with the same qualifier in a descending category.
	 *
	 * @param product product to get settings for
	 * @return list of configuration settings. The list items are unique relative to their {@code qualifier}.
	 */
	@Nonnull
	List<AbstractConfiguratorSettingModel> getConfiguratorSettingsForProduct(@Nonnull ProductModel product);
}
