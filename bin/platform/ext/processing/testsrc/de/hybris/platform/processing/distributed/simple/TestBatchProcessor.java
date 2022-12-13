/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.processing.distributed.simple;

import de.hybris.platform.core.PK;
import de.hybris.platform.processing.model.SimpleBatchModel;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.List;

import org.springframework.beans.factory.annotation.Required;

import com.google.common.base.Preconditions;


public class TestBatchProcessor implements SimpleBatchProcessor
{
	private ModelService modelService;

	@Override
	public void process(final SimpleBatchModel inputBatch)
	{
		final List<PK> pksToRemove = asList(inputBatch.getContext());

		pksToRemove.stream().forEach(pk -> modelService.remove(pk));
	}

	protected List<PK> asList(final Object ctx)
	{
		Preconditions.checkState(ctx instanceof List, "ctx must be instance of List");

		return (List<PK>) ctx;
	}

	@Required
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}
}
