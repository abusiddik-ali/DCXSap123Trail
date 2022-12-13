/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.media.web;

import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.media.MediaFolderModel;
import de.hybris.platform.jalo.media.MediaManager;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import javax.servlet.http.HttpServletResponse;


public class TestDefaultMediaFilterLogic extends DefaultMediaFilterLogic
{
	public TestDefaultMediaFilterLogic()
		{
			super(null, null);
		}

	public TestDefaultMediaFilterLogic(final FlexibleSearchService flexibleSearchService)
	{
		super(flexibleSearchService, null);
	}

	@Override
	protected String getContextPart(final MediaFilterLogicContext.ContextPart contextPart, final Iterable<String> mediaContext)
	{
		final List<String> part = new ArrayList<>();
		mediaContext.forEach(part::add);
		return part.get(contextPart.getPartNumber());
	}

	public void addContentType(final HttpServletResponse httpResponse, final Iterable<String> mediaContext,
			final String resourcePath)
	{
		super.addContentType(httpResponse, new MediaFilterLogicContext(mediaContext, resourcePath));
	}

	@Override
	public Optional<String> getPersistedContentType(final String folderQualifier, final String location)
	{
		return super.getPersistedContentType(folderQualifier, location);
	}
	
	@Override
	public List<String> loadMimesFromDb(final PK pk)
	{
		return super.loadMimesFromDb(pk);
	}

	@Override
	public Optional<PK> extractDataPKFromLocation(final String location)
	{
		return super.extractDataPKFromLocation(location);
	}

	@Override
	public MediaManager.InputStreamWithSize getMediaAsStreamWithSize(final String folderQualifier, final String location)
	{
		return super.getMediaAsStreamWithSize(folderQualifier, location);
	}
}