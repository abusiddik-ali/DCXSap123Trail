/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jobs;

import de.hybris.platform.core.LazyLoadItemList;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.suspend.SuspendResumeService;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.jobs.maintenance.MaintenanceCleanupStrategy;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import de.hybris.platform.servicelayer.cronjob.TypeAwareJobPerformable;
import de.hybris.platform.servicelayer.internal.model.MaintenanceCleanupJobModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.servicelayer.search.impl.LazyLoadModelList;
import de.hybris.platform.servicelayer.search.internal.resolver.ItemObjectResolver;
import de.hybris.platform.util.Config;

import java.util.Arrays;
import java.util.List;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;


/**
 * Job Performable which executes a given {@link MaintenanceCleanupStrategy}. With the given Search result from
 * {@link MaintenanceCleanupStrategy#createFetchQuery(CronJobModel)} the job pages throught the resultlist (list of PKs,
 * small) and for each sublist (100 elements as default, modify with {@link #setPageSize(int)}) the
 * {@link MaintenanceCleanupStrategy#process(List)} is executed. If {@link #setAbortOnError(boolean)} is set to true
 * this performable will stop when during {@link MaintenanceCleanupStrategy#process(List)} an exception is thrown and
 * the performable stops with an error/aborted. When set to false (default) the exception is just logged.
 */
public abstract class AbstractMaintenanceJobPerformable extends AbstractJobPerformable<CronJobModel>
		implements TypeAwareJobPerformable
{
	private final static Logger LOG = Logger.getLogger(AbstractMaintenanceJobPerformable.class.getName());
	private ItemObjectResolver modelResolver;
	private boolean abortOnError = false;
	private int pageSize = 100;
	private SuspendResumeService suspendResumeService;

	/**
	 * @see MaintenanceCleanupStrategy#createFetchQuery(CronJobModel)
	 */
	public abstract FlexibleSearchQuery getFetchQuery(final CronJobModel cronJob);

	/**
	 * @see MaintenanceCleanupStrategy#process(List)
	 */
	public abstract void process(final List<ItemModel> elements, final CronJobModel cronJob);


	@Override
	public final PerformResult perform(final CronJobModel cronJob)
	{
		boolean caughtExeption = false;

		if (shouldAbort())
		{
			if (LOG.isDebugEnabled())
			{
				LOG.debug("System suspend in progress, aborting maintenance job.");
			}
			return new PerformResult(CronJobResult.UNKNOWN, CronJobStatus.ABORTED);
		}

		//first, get the query from the strategy
		final FlexibleSearchQuery createFetchQuery = getFetchQuery(cronJob);
		if (createFetchQuery == null)
		{
			throw new IllegalStateException("The FlexibleSearchQuery object was null, cannot procceed!");
		}

		//and remember what the strategy wants as result class
		//if nothing is set in the interface impl, this list contains Item.class as default value
		final List<Class> expectedClassList = createFetchQuery.getResultClassList();

		//before we search, we'll overwrite this anyway because PKs are smaller
		createFetchQuery.setResultClassList(Arrays.asList(PK.class));

		//do the search, result should be a lazyloadedlist with PKs
		final SearchResult<PK> searchRes = flexibleSearchService.search(createFetchQuery);


		//paging through the PKs and for each page, we get the real deal
		final int totalCount = searchRes.getTotalCount();
		for (int i = 0; i < totalCount; i += pageSize)
		{
			if (shouldAbort())
			{
				if (LOG.isDebugEnabled())
				{
					LOG.debug("System suspend in progress, aborting maintenance job.");
				}
				return new PerformResult(CronJobResult.UNKNOWN, CronJobStatus.ABORTED);
			}
			final List<PK> sublist = searchRes.getResult().subList(i, Math.min(i + pageSize, totalCount));
			final LazyLoadModelList llml = new LazyLoadModelList(new LazyLoadItemList(null, sublist, pageSize), pageSize,
					expectedClassList, modelResolver);
			try
			{
				process(llml, cronJob);
				for (final Object obj : llml)
				{
					if (obj != null)
				{
					modelService.detach(obj);
				}
			}
			}
			catch (final Exception e)
			{
				caughtExeption = true;
				LOG.error("Caught exception during process call. " + e.getClass().getName() + ": " + e.getMessage());
				if (abortOnError)
				{
					LOG.error("stacktrace:", e);
					return new PerformResult(CronJobResult.ERROR, CronJobStatus.ABORTED);
				}
			}
		}
		return new PerformResult(caughtExeption ? CronJobResult.FAILURE : CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	protected boolean shouldAbort()
	{
		return Config.getBoolean("abort.maintenance.job.during.suspend", true) 
				&& !getSuspendResumeService().getSystemState().getStatus().isRunningOrWaitingForUpdate();
	}

	@Required
	public void setModelResolver(final ItemObjectResolver modelResolver)
	{
		this.modelResolver = modelResolver;
	}

	/**
	 * Sets the page size for the removing part. Default is 100. If set to 80 the elements list in
	 * {@link #process(List, CronJobModel)} will contain 80 elements or less.
	 */
	public void setPageSize(final int pagesize)
	{
		if (pagesize < 0)
		{
			throw new IllegalArgumentException("pagesize cannot be negative");
		}
		this.pageSize = pagesize;
	}

	/**
	 * If set to true the job will abort by the first thrown exception. When set to false (default) the exception is just
	 * logged.
	 */
	public void setAbortOnError(final boolean abort)
	{
		this.abortOnError = abort;
	}

	@Override
	public String getType()
	{
		return MaintenanceCleanupJobModel._TYPECODE;
	}

	public void setSuspendResumeService(final SuspendResumeService suspendResumeService)
	{
		this.suspendResumeService = suspendResumeService;
	}
	
	private SuspendResumeService getSuspendResumeService()
	{
		if (suspendResumeService == null)
		{
			suspendResumeService = Registry.getApplicationContext().getBean("suspendResumeService", SuspendResumeService.class);
		}

		return suspendResumeService;
	}
}
