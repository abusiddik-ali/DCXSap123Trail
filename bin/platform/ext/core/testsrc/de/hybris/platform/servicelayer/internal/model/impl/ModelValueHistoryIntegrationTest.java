/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.internal.model.impl;

import static de.hybris.platform.servicelayer.model.ModelContextUtils.getItemModelContext;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelContextUtils;
import de.hybris.platform.servicelayer.model.ModelService;

import javax.annotation.Resource;

import org.junit.Assert;
import org.junit.Test;


/**
 *
 */
@IntegrationTest
public class ModelValueHistoryIntegrationTest extends ServicelayerBaseTest
{

	private static final String ORIGINAL_CODE_VALUE = "new Code";

	@Resource
	private ModelService modelService;

	@Test
	public void testCheckDirtyAttributesForUnattachedModel()
	{

		final TitleModel newModel = new TitleModel();
		newModel.setCode(ORIGINAL_CODE_VALUE);


		Assert.assertTrue(getItemModelContext(newModel).isDirty(TitleModel.CODE));

		Assert.assertTrue(modelService.isModified(newModel));
		Assert.assertFalse(modelService.isUpToDate(newModel));
	}


	@Test
	public void testCheckDirtyAttributesForAttachedModel()
	{

		final TitleModel newModel = new TitleModel();
		modelService.attach(newModel);

		newModel.setCode(ORIGINAL_CODE_VALUE);

		Assert.assertTrue(getItemModelContext(newModel).isDirty(TitleModel.CODE));

		Assert.assertTrue(modelService.isModified(newModel));
		Assert.assertFalse(modelService.isUpToDate(newModel));
	}


	@Test
	public void testCheckDirtyAttributesAfterSave()
	{

		final TitleModel newModel = new TitleModel();
		newModel.setCode(ORIGINAL_CODE_VALUE);

		Assert.assertTrue(ModelContextUtils.getItemModelContext(newModel).isDirty(TitleModel.CODE));
		Assert.assertTrue(modelService.isModified(newModel));
		Assert.assertFalse(modelService.isUpToDate(newModel));

		modelService.attach(newModel);
		modelService.save(newModel);

		Assert.assertFalse(ModelContextUtils.getItemModelContext(newModel).isDirty(TitleModel.CODE));
		Assert.assertFalse(modelService.isModified(newModel));
		Assert.assertTrue(modelService.isUpToDate(newModel));

	}


	@Test
	public void testCheckDirtyAttributesSaveModifyAfter()
	{

		final TitleModel newModel = new TitleModel();
		newModel.setCode(ORIGINAL_CODE_VALUE);

		modelService.attach(newModel);
		modelService.save(newModel);

		newModel.setCode("new Code changed");

		Assert.assertTrue(getItemModelContext(newModel).isDirty(TitleModel.CODE));
		Assert.assertTrue(modelService.isModified(newModel));
		Assert.assertFalse(modelService.isUpToDate(newModel));

	}


	@Test
	public void testCheckDirtyAttributesSaveDetachModifyAfter()
	{

		final TitleModel newModel = new TitleModel();
		newModel.setCode(ORIGINAL_CODE_VALUE);

		modelService.attach(newModel);
		modelService.save(newModel);

		modelService.detach(newModel);

		Assert.assertFalse(getItemModelContext(newModel).isDirty(TitleModel.CODE));
		Assert.assertFalse(modelService.isModified(newModel));
		Assert.assertTrue(modelService.isUpToDate(newModel));

		newModel.setCode("new Code changed");

		Assert.assertTrue(getItemModelContext(newModel).isDirty(TitleModel.CODE));
		Assert.assertTrue(modelService.isModified(newModel));
		Assert.assertFalse(modelService.isUpToDate(newModel));

	}
}
