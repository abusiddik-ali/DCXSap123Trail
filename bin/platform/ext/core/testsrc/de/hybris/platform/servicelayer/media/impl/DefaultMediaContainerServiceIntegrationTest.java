/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.media.impl;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;

import javax.annotation.Resource;


@IntegrationTest
public class DefaultMediaContainerServiceIntegrationTest extends AbstractMediaFormatIntegrationTest
{

	@Resource
	private DefaultMediaContainerService mediaContainerService;


	@Override
	MediaModel getMediaByFormat(final MediaModel last, final MediaFormatModel mediaFormatModel)
	{
		// this is redundant and useless, but the MEdiaContainerService does not host this method
		return this.getMediaByFormat(last.getMediaContainer(), mediaFormatModel);
	}

	@Override
	MediaModel getMediaByFormat(final MediaContainerModel container, final MediaFormatModel mediaFormatModel)
	{
		return this.mediaContainerService.getMediaForFormat(container, mediaFormatModel);
	}

}
