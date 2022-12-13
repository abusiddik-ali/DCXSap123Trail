/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.servicelayer;


import de.hybris.platform.constants.CoreConstants;
import de.hybris.platform.core.initialization.SystemSetup;
import de.hybris.platform.impex.jalo.ImpExManager;
import de.hybris.platform.util.CSVConstants;
import de.hybris.platform.util.Config;

import java.io.InputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@SystemSetup(extension = CoreConstants.EXTENSIONNAME)
public class CleanUpCronJobHistoryService
{
	private static final String CLEANUP_CRON_JOB_HISTORY_IMPEX = "/impex/cleanup-cronjobhistory.impex";
	private static final Logger LOG = LoggerFactory.getLogger(CleanUpCronJobHistoryService.class);
	private static final String IMPEX_LEGACY_SCRIPTING = "impex.legacy.scripting";

	@SystemSetup(type = SystemSetup.Type.ESSENTIAL, process = SystemSetup.Process.ALL)
	public void initializeCleanUpForCronJobHistory()
	{
		final String legacyScriptingPreviousValue = ensureImpexLegacyScriptingIsTrueAndReturnPreviousValue();

		importCleanUpCronJobHistoryImpex();

		setLegacyScriptingPropertyValue(legacyScriptingPreviousValue);
	}

	private void importCleanUpCronJobHistoryImpex()
	{
		LOG.info("Importing resource: {}", CLEANUP_CRON_JOB_HISTORY_IMPEX);
		try (final InputStream impexStream = getCleanUpImpexAsStream())
		{
			importStream(impexStream);
		}
		catch (final Exception e)
		{
			final String failMsg = "Failed to import: " + CLEANUP_CRON_JOB_HISTORY_IMPEX;
			LOG.warn(failMsg);
			LOG.debug(failMsg, e);
		}
	}

	private void importStream(final InputStream impexStream)
	{
		ImpExManager.getInstance().importData(impexStream, CSVConstants.HYBRIS_ENCODING, CSVConstants.HYBRIS_FIELD_SEPARATOR,
				CSVConstants.HYBRIS_QUOTE_CHARACTER, true);
	}

	private InputStream getCleanUpImpexAsStream()
	{
		return CleanUpCronJobHistoryService.class.getResourceAsStream(CLEANUP_CRON_JOB_HISTORY_IMPEX);
	}

	private String ensureImpexLegacyScriptingIsTrueAndReturnPreviousValue()
	{
		final String legacyScriptingProperty = Config.getParameter(IMPEX_LEGACY_SCRIPTING);
		Config.setParameter(IMPEX_LEGACY_SCRIPTING, Boolean.TRUE.toString());
		return legacyScriptingProperty;
	}

	private void setLegacyScriptingPropertyValue(final String value)
	{
		Config.setParameter(IMPEX_LEGACY_SCRIPTING, value);
	}
}
