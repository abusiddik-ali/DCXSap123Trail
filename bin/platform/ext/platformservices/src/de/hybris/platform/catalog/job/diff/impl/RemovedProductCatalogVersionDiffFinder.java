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
 * Implementation for a finding removed {@link ProductModel} between two {@link CatalogVersionModel}s.
 */
public class RemovedProductCatalogVersionDiffFinder extends AbstractProductCatalogVersionDiffFinder
{

	@Override
	public Collection<ProductModel> findDifferences(final int start, final int count,
	                                                final CompareCatalogVersionsCronJobModel model)
	{
		return search4Diffs(start, count, model.getTargetVersion(), model.getSourceVersion());
	}

	@Override
	protected ProductDifferenceMode getProductDifferenceMode()
	{
		return (ProductDifferenceMode) enumerationService.getEnumerationValue(ProductDifferenceMode.PRODUCT_REMOVED.getType(),
				ProductDifferenceMode.PRODUCT_REMOVED.getCode());
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

		differenceModel.setSourceProduct(srcProduct);
		differenceModel.setTargetProduct(null);

		differenceModel.setDifferenceText("Product " + srcProduct.getCode() + " not found in version: "
				+ model.getTargetVersion().getVersion());
		return differenceModel;
	}


	@Override
	protected void setDifferencesCount(final CompareCatalogVersionsCronJobModel model, final int differencedProductsCount)
	{
		model.setMissingProducts(differencedProductsCount);
		modelService.save(model);
	}

	@Override
	protected boolean shouldProcess(final CompareCatalogVersionsCronJobModel cronJobModel)
	{
		return BooleanUtils.isTrue(cronJobModel.getSearchMissingProducts());
	}

}
