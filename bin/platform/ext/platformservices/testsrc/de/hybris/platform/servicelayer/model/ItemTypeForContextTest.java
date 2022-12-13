/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.model;

import static org.junit.Assert.assertEquals;

import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.jalo.JaloInvalidParameterException;
import de.hybris.platform.jalo.type.ComposedType;
import de.hybris.platform.jalo.type.JaloDuplicateCodeException;
import de.hybris.platform.jalo.type.TypeManager;
import de.hybris.platform.jalo.user.Title;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import javax.annotation.Resource;

import org.junit.Test;


public class ItemTypeForContextTest extends ServicelayerBaseTest
{
	@Resource
	private ModelService modelService;

	@Test
	public void testItemTypeWithoutModelClass() throws JaloInvalidParameterException, JaloDuplicateCodeException
	{
		final TypeManager tm = TypeManager.getInstance();
		final ComposedType titleType = tm.getComposedType(Title.class);
		final ComposedType myTtitletype = tm.createComposedType(titleType, "MyTitle");

		final TitleModel title1 = modelService.create(TitleModel.class);
		title1.setCode("t1");
		final TitleModel title2 = modelService.create(titleType.getCode());
		title2.setCode("t2");
		final TitleModel title3 = modelService.create(myTtitletype.getCode());
		title3.setCode("t3");

		assertEquals(titleType.getCode(), title2.getItemtype());
		assertEquals(titleType.getCode(), title1.getItemtype());
		assertEquals(myTtitletype.getCode(), title3.getItemtype());

		modelService.saveAll(title1, title2, title3);

		assertEquals(titleType.getCode(), title2.getItemtype());
		assertEquals(titleType.getCode(), title1.getItemtype());
		assertEquals(myTtitletype.getCode(), title3.getItemtype());
	}
}
