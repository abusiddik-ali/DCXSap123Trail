/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.scripting.events.impl;

import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.scripting.events.TestPerformanceEvent;
import de.hybris.platform.servicelayer.event.events.AbstractEvent;
import de.hybris.platform.servicelayer.event.impl.AbstractEventListener;
import de.hybris.platform.servicelayer.model.ModelService;


/**
 * test standard (non scripting) listener for performance tetss
 */
public class TestStandardEventListener extends AbstractEventListener
{
	@Override
	protected void onEvent(final AbstractEvent event)
	{
		if (event instanceof TestPerformanceEvent)
		{
			final ModelService modelService = (ModelService) Registry.getApplicationContext().getBean("modelService");
			for (int i = 0; i < ((TestPerformanceEvent) event).getItemsToSaveCount(); i++)
			{
				final TitleModel title = modelService.create(TitleModel.class);
				title.setCode("testTitle" + i);
				modelService.save(title);
			}
		}
		else
		{
			Math.random();
		}
	}
}
