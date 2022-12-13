/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.job.diff.impl;

import de.hybris.platform.catalog.enums.ProductDifferenceMode;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.catalog.model.CompareCatalogVersionsCronJobModel;
import de.hybris.platform.catalog.model.ProductCatalogVersionDifferenceModel;
import de.hybris.platform.core.model.product.ProductModel;

import java.util.Collection;

import org.apache.commons.lang.BooleanUtils;


/**
 * Finder for a new {@link ProductModel} differences between two {@link CatalogVersionModel}s from given
 * {@link CompareCatalogVersionsCronJobModel}.
 */
public class NewProductCatalogVersionDiffFinder extends AbstractProductCatalogVersionDiffFinder
{


	@Override
	public Collection<ProductModel> findDifferences(final int start, final int count,
	                                                final CompareCatalogVersionsCronJobModel model)
	{
		return search4Diffs(start, count, model.getSourceVersion(), model.getTargetVersion());
	}

	@Override
	protected ProductDifferenceMode getProductDifferenceMode()
	{
		return (ProductDifferenceMode) enumerationService.getEnumerationValue(ProductDifferenceMode.PRODUCT_NEW.getType(),
				ProductDifferenceMode.PRODUCT_NEW.getCode());
	}

	@Override
	public ProductCatalogVersionDifferenceModel populateDifferenceModel(final ProductModel srcProduct,
	                                                                    final ProductModel targetProduct,
	                                                                    final CompareCatalogVersionsCronJobModel model)
	{
		final ProductCatalogVersionDifferenceModel differenceModel = modelService
				.create(ProductCatalogVersionDifferenceModel.class);
		differenceModel.setSourceVersion(model.getSourceVersion());
		differenceModel.setTargetVersion(model.getTargetVersion());
		differenceModel.setCronJob(model);
		differenceModel.setMode(getProductDifferenceMode());

		differenceModel.setSourceProduct(null);
		differenceModel.setTargetProduct(srcProduct);

		differenceModel.setDifferenceText("Product " + srcProduct.getCode() + " new in version: "
				+ model.getTargetVersion().getVersion());
		return differenceModel;
	}

	@Override
	protected void setDifferencesCount(final CompareCatalogVersionsCronJobModel model, final int differencedProductsCount)
	{
		model.setNewProducts(differencedProductsCount);
		modelService.save(model);
	}

	@Override
	protected boolean shouldProcess(final CompareCatalogVersionsCronJobModel cronJobModel)
	{
		return BooleanUtils.isTrue(cronJobModel.getSearchNewProducts());
	}

}
