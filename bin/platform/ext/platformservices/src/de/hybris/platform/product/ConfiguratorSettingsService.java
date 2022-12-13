/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.product;


import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.product.model.AbstractConfiguratorSettingModel;

import java.util.List;

import javax.annotation.Nonnull;


/**
 * Encapsulates configurator-settings related logic.
 *
 * @see AbstractConfiguratorSettingModel
 * @see de.hybris.platform.category.model.ConfigurationCategoryModel
 */
public interface ConfiguratorSettingsService
{
	/**
	 * Returns configurator settings to be applied to given product.
	 * <p>
	 * Settings are collected from all categories assigned to the product including the whole hierarchy. When two
	 * categories have equal Id, only one which is closer to the product is taken. The method keeps setting item order
	 * from root to leaf categories.
	 * </p>
	 *
	 * @param product product to get settings for
	 * @return list of configurator settings
	 * @see ConfiguratorSettingsResolutionStrategy
	 * @see de.hybris.platform.category.model.ConfigurationCategoryModel
	 * @see de.hybris.platform.category.CategoryService
	 */
	@Nonnull
	List<AbstractConfiguratorSettingModel> getConfiguratorSettingsForProduct(@Nonnull ProductModel product);
}
