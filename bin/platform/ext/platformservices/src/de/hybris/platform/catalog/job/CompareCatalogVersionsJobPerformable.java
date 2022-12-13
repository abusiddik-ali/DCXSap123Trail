/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.job;

import de.hybris.platform.catalog.job.diff.CatalogVersionDifferenceFinder;
import de.hybris.platform.catalog.model.CompareCatalogVersionsCronJobModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.JobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;

import java.util.List;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;


/**
 * {@link JobPerformable} implementation for comparing two catalog versions.
 *
 * @spring.bean compareCatalogVersionsJobPerformable
 * @since 4.3
 */
public class CompareCatalogVersionsJobPerformable extends AbstractJobPerformable<CompareCatalogVersionsCronJobModel>
{
	private static final Logger LOG = Logger.getLogger(CompareCatalogVersionsJobPerformable.class.getName());

	private List<CatalogVersionDifferenceFinder> compareSteps;

	@Required
	public void setCompareSteps(final List<CatalogVersionDifferenceFinder> compareSteps)
	{
		this.compareSteps = compareSteps;
	}

	@Override
	public PerformResult perform(final CompareCatalogVersionsCronJobModel cronJobModel)
	{

		int processedCounter = 0;
		for (final CatalogVersionDifferenceFinder finder : compareSteps)
		{
			processedCounter += finder.processDifferences(cronJobModel);
			if (LOG.isDebugEnabled())
			{
				LOG.debug("Processed entries after find step is :" + processedCounter);
			}
		}

		cronJobModel.setProcessedItemsCount(processedCounter);
		modelService.save(cronJobModel);

		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

}
