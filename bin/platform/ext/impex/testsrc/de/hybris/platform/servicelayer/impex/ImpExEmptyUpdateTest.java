/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.impex;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.Tenant;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.core.threadregistry.RegistrableThread;
import de.hybris.platform.jalo.ExtensibleItem;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.jalo.c2l.Country;
import de.hybris.platform.jalo.user.Title;
import de.hybris.platform.persistence.hjmp.HJMPUtils;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.internal.model.extractor.impl.DefaultPersistenceTypeService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.tx.Transaction;
import de.hybris.platform.tx.TransactionBody;
import de.hybris.platform.util.Config;

import java.util.Collections;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import javax.annotation.Resource;

import org.junit.Test;


/**
 * ECP-595
 */
@IntegrationTest
public class ImpExEmptyUpdateTest extends ServicelayerBaseTest
{
	private static final String CODE = "TESTCOUNTRY";

	@Resource
	private ImportService importService;

	@Resource
	CommonI18NService commonI18NService;

	@Resource
	ModelService modelService;

	@Test
	public void testEmptyUpdateMultiThreadedServiceLayer()
	{
		if (Config.getBoolean(DefaultPersistenceTypeService.PERSISTENCE_LEGACY_MODE, false))
		{
			testEmptyUpdateImpl(false, 3);
		}
		else
		{
			System.err.println("##############################################################################");
			System.err.println("# HORST-1680 SLD WRITE can't deal with empty updates yet - skipping test !!! #");
			System.err.println("##############################################################################");
		}
	}

	@Test
	public void testEmptyUpdateMultiThreadedJalo()
	{
		testEmptyUpdateImpl(true, 3);
	}

	@Test
	public void testEmptyUpdateSingleThreadedServiceLayer()
	{
		if (Config.getBoolean(DefaultPersistenceTypeService.PERSISTENCE_LEGACY_MODE, false))
		{
			testEmptyUpdateImpl(false, 1);
		}
		else
		{
			System.err.println("##############################################################################");
			System.err.println("# HORST-1680 SLD WRITE can't deal with empty updates yet - skipping test !!! #");
			System.err.println("##############################################################################");
		}
	}

	@Test
	public void testEmptyUpdateSingleThreadedJalo()
	{
		testEmptyUpdateImpl(true, 1);
	}

	@Test
	public void testOptimisticLockingStillWorks()
	{
		final CountryModel model = createModel("1");
		final Country jaloItem = modelService.getSource(model);

		withOptimisticLocking(() -> {
			simulateJaloWritesGetTrackedCorrectly(model, jaloItem);
		});
	}

	@Test
	public void testOptimisticLockingStillWorksAndDetectsConcurrentUpdate()
	{
		final CountryModel model = createModel("2");
		final Country jaloItem = modelService.getSource(model);

		withOptimisticLocking(() -> {
			simulateJaloWritesGetTrackedCorrectly(model, jaloItem);
			simulateConcurrentUpdateMustFail(model, jaloItem, true);
		});
	}


	@Test
	public void testOptimisticLockingStillWorksAndDetectsConcurrentUpdateToSameValue()
	{
		final CountryModel model = createModel("3");
		final Country jaloItem = modelService.getSource(model);

		withOptimisticLocking(() -> {
			simulateJaloWritesGetTrackedCorrectly(model, jaloItem);
			simulateConcurrentUpdateToSameValueMustFail(model, jaloItem, true);
		});
	}


	@Test
	public void testOptimisticLockingIsBlacklistedForConfiguredCountryType()
	{
		final CountryModel model = createModel("4");
		final Country jaloItem = modelService.getSource(model);

		withOptimisticLockingDisabledForType(() -> {
			simulateJaloWritesGetTrackedCorrectly(model, jaloItem);
			simulateConcurrentUpdateToSameValueMustNotFail(model, jaloItem);
		},"Country");
	}


	@Test
	public void testOptimisticLockingIsBlacklistedForConfiguredCountryTypeButWorksForTitleType()
	{
		final CountryModel model = createModel("5");
		final Country jaloItem = modelService.getSource(model);
		final TitleModel titleModel = createTitleModel("5");
		final Title jaloTitleItem = modelService.getSource(titleModel);

		withOptimisticLockingDisabledForType(() -> {
			simulateJaloWritesGetTrackedCorrectly(model, jaloItem);
			simulateConcurrentUpdateToSameValueMustNotFail(model, jaloItem);
			simulateJaloWritesGetTrackedCorrectly(titleModel, jaloTitleItem);
			simulateConcurrentUpdateMustFail(titleModel, jaloTitleItem,true);

		},"Country");
	}



	@Test
	public void testOptimisticLockingIsBlacklistedForConfiguredCountryAndTitleType()
	{
		final CountryModel model = createModel("6");
		final Country jaloItem = modelService.getSource(model);
		final TitleModel titleModel = createTitleModel("6");
		final Title jaloTitleItem = modelService.getSource(titleModel);

		withOptimisticLockingDisabledForType(() -> {
			simulateJaloWritesGetTrackedCorrectly(model, jaloItem);
			simulateConcurrentUpdateToSameValueMustNotFail(model, jaloItem);
			simulateJaloWritesGetTrackedCorrectly(titleModel, jaloTitleItem);
			simulateConcurrentUpdateToSameValueMustNotFail(titleModel, jaloTitleItem);

		},"Country,Title");
	}

	@Test
	public void testOptimisticLockingWhereCountryTypeIsConfiguredAsBlacklistedInOneThreadAndNotConfiguredInAnotherThread()
	{
		final CountryModel model = createModel("12");
		final Country jaloItem = modelService.getSource(model);

		withOptimisticLockingDisabledForType(() -> {
			simulateJaloWritesGetTrackedCorrectly(model, jaloItem);
			simulateConcurrentUpdateToSameValueMustNotFail(model, jaloItem);

		},"Country");


		modelService.refresh(model);

		withOptimisticLocking(() -> {
			simulateJaloWritesGetTrackedCorrectly(model, jaloItem);
			simulateConcurrentUpdateMustFail(model, jaloItem,true);
		});

	}
	@Test
	public void testOptimisticLockingStillWorksTX() throws Exception
	{
		final CountryModel model = createModel("7");
		final Country jaloItem = modelService.getSource(model);

		withOptimisticLocking(() -> inTransaction(() -> simulateJaloWritesGetTrackedCorrectly(model, jaloItem)));

	}

	@Test
	public void testOptimisticLockingStillWorksAndDetectsConcurrentUpdateTX()
	{
		final CountryModel model = createModel("11");
		final Country jaloItem = modelService.getSource(model);

		withOptimisticLocking(() -> inTransactionWhichFails(() -> {
			simulateJaloWritesGetTrackedCorrectly(model, jaloItem);
			simulateConcurrentUpdateMustFail(model, jaloItem, false);
		}));
	}

	@Test
	public void testOptimisticLockingEnabledButNotDetectsConcurrentUpdateTXForBlacklistedType()
	{
		final CountryModel model = createModel("9");
		final Country jaloItem = modelService.getSource(model);

		withOptimisticLockingDisabledForType(() -> inTransactionWhichFails(() -> {
			simulateJaloWritesGetTrackedCorrectly(model, jaloItem);
			simulateConcurrentUpdateToSameValueMustNotFail(model, jaloItem);
		}),"Country");
	}

	@Test
	public void testOptimisticLockingStillWorksAndDetectsConcurrentUpdateToSameValueTX() throws Exception
	{
		final CountryModel model = createModel("10");
		final Country jaloItem = modelService.getSource(model);

		withOptimisticLocking(() -> inTransactionWhichFails(() -> {
			simulateJaloWritesGetTrackedCorrectly(model, jaloItem);
			simulateConcurrentUpdateToSameValueMustFail(model, jaloItem, false);
		}));
	}


	protected void withOptimisticLocking(final Runnable r)
	{
		try
		{
			HJMPUtils.enableOptimisticLocking();
			r.run();
		}
		finally
		{
			HJMPUtils.clearOptimisticLockingSetting();
		}
	}

	protected void withOptimisticLockingDisabledForType(final Runnable r, final String typeToSet)
	{

		HJMPUtils.setBlacklistedTypesForOptimisticLocking(typeToSet);
		try
		{
			HJMPUtils.enableOptimisticLocking();
			r.run();
		}
		finally
		{
			HJMPUtils.clearOptimisticLockingSetting();
		}


	}

	protected void inTransaction(final Runnable r)
	{
		try
		{
			final Transaction tx = Transaction.current();
			tx.execute(new TransactionBody()
			{
				@Override
				public <T> T execute() throws Exception
				{
					r.run();
					return null;
				}
			});
		}
		catch (final Exception e)
		{
			throw new RuntimeException(e);
		}
	}

	protected void inTransactionWhichFails(final Runnable r)
	{
		try
		{
			inTransaction(r);
			fail("transaction was supposed to fail but didnt!");
		}
		catch (final Exception e)
		{
			// fine here
		}
	}

	protected CountryModel createModel(final String suffixName)
	{
		final CountryModel model = modelService.create(CountryModel.class);
		model.setIsocode("FOOBAR" + suffixName);
		model.setActive(Boolean.FALSE);
		modelService.save(model);
		return model;
	}

	protected TitleModel createTitleModel(final String suffixName)
	{
		final TitleModel model = modelService.create(TitleModel.class);
		model.setCode("testCode" + suffixName);
		model.setName("testCodeName"+suffixName);
		modelService.save(model);
		return model;
	}

	protected void simulateConcurrentUpdateToSameValueMustFail(final CountryModel model, final Country jaloItem,
	                                                           final boolean mustFailImmediately)
	{
		// given
		simulateConcurrentUpdateToSameValue(model, jaloItem);

		try
		{
			jaloItem.setProperty("propertyForBoth", "newValue");
			if (mustFailImmediately)
			{
				fail("optimistic locking via Service Layer didn't detect concurrent update");
			}
		}
		catch (final Exception e)
		{
			// expected

		}
	}

	private void simulateConcurrentUpdateToSameValue(final ItemModel model, final ExtensibleItem jaloItem)
	{
		assertTrue(HJMPUtils.isOptimisticLockingEnabled());

		jaloItem.setProperty("propertyForBoth", "startValue");

		final Long trackedVersionFromSL = HJMPUtils.getVersionForPk(model.getPk());
		assertNotNull(trackedVersionFromSL);

		// when
		final long versionFromOtherThread = changeInOtherThread(model.getPk(), "propertyForBoth", "newValue");

		// then
		assertEquals(trackedVersionFromSL.longValue() + 1, versionFromOtherThread);
	}


	protected void simulateConcurrentUpdateToSameValueMustNotFail(final ItemModel model, final ExtensibleItem jaloItem)
	{
		// given
		simulateConcurrentUpdateToSameValue(model, jaloItem);

		try
		{
			jaloItem.setProperty("propertyForBoth", "newValue");
		}
		catch (final Exception e)
		{
			fail("optimistic locking via Service Layer detect concurrent update");

		}
	}

	protected void simulateConcurrentUpdateMustFail(final ItemModel model, final ExtensibleItem jaloItem,
	                                                final boolean mustFailImmediately)
	{
		// given
		assertTrue(HJMPUtils.isOptimisticLockingEnabled());
		final Long trackedVersionFromSL = HJMPUtils.getVersionForPk(model.getPk());
		assertNotNull(trackedVersionFromSL);

		// when
		final long versionFromOtherThread = changeInOtherThread(model.getPk(), "someProperty", "someValue");

		// then
		assertEquals(trackedVersionFromSL.longValue() + 1, versionFromOtherThread);

		try
		{
			jaloItem.setProperty("yetAnotherProperty", "shouldFailNow");
			if (mustFailImmediately)
			{
				fail("optimistic locking via Service Layer didn't detect concurrent update");
			}
		}
		catch (final Exception e)
		{
			// expected
		}
	}

	protected void simulateJaloWritesGetTrackedCorrectly(final ItemModel model, final ExtensibleItem jaloItem)
	{
		final long versionFromSL = model.getItemModelContext().getPersistenceVersion();
		final long versionFromJalo = jaloItem.getPersistenceVersion();

		assertEquals(versionFromSL, versionFromJalo);

		// simulate multiple writes via Jalo from model service
		HJMPUtils.registerVersionsForPks(Collections.singletonMap(model.getPk(), Long.valueOf(versionFromSL)));

		for (int i = 0; i < 10; i++)
		{
			jaloItem.setProperty("xxx", Integer.valueOf(i));
		}

		final long versionFromJaloAfter = jaloItem.getPersistenceVersion();
		final Long trackedVersionFromSL = HJMPUtils.getVersionForPk(model.getPk());

		assertNotNull(trackedVersionFromSL);
		assertEquals(trackedVersionFromSL.longValue(), versionFromJaloAfter);

	}

	protected long changeInOtherThread(final PK itemPK, final String property, final Object value)
	{
		return runInOtherThread(new Callable<Long>()
		{
			@Override
			public Long call() throws Exception
			{
				final ExtensibleItem jaloItem = JaloSession.getCurrentSession().getItem(itemPK);
				jaloItem.setProperty(property, value);

				return Long.valueOf(jaloItem.getPersistenceVersion());
			}
		}, 10).longValue();
	}

	protected void testEmptyUpdateImpl(final boolean legacyMode, final int threads)
	{
		// given
		importService.importData(setUpImport(legacyMode, threads));
		final ItemModel itemFromImport = getItemFromImport();
		final long timestamp = itemFromImport.getItemModelContext().getPersistenceVersion();

		// when
		importService.importData(setUpImport(legacyMode, threads));
		final ItemModel itemFromEmptyUpdate = getItemFromImport();
		final long timestampAfterEmptyUpdate = itemFromEmptyUpdate.getItemModelContext().getPersistenceVersion();

		// then
		assertEquals("empty update wrongly changed item version", timestamp, timestampAfterEmptyUpdate);

	}

	ItemModel getItemFromImport()
	{
		// make sure we dont fetch the *same* model instance !!!
		modelService.detachAll();
		return commonI18NService.getCountry(CODE);
	}

	ImportConfig setUpImport(final boolean legacyMode, final int threads)
	{
		final StringBuilder sb = new StringBuilder();
		sb.append("INSERT_UPDATE Country; " + CountryModel.ISOCODE + "[unique=true]; " + CountryModel.ACTIVE + ";").append('\n');
		sb.append(";" + CODE + ";false;").append('\n');

		final ImportConfig cfg = new ImportConfig();
		cfg.setLegacyMode(Boolean.valueOf(legacyMode));
		cfg.setMaxThreads(threads);
		cfg.setScript(sb.toString());

		return cfg;
	}

	private <V> V runInOtherThread(final Callable<V> callable, final int timeoutSeconds)
	{
		final ExecutorService pool = Executors.newFixedThreadPool(1, new ThreadFactory()
		{
			final Tenant tenant = Registry.getCurrentTenantNoFallback();

			@Override
			public Thread newThread(final Runnable r)
			{
				return new RegistrableThread(r)
				{
					@Override
					public void internalRun()
					{
						try
						{
							Registry.setCurrentTenant(tenant);
							super.internalRun();
						}
						finally
						{
							JaloSession.deactivate();
							Registry.unsetCurrentTenant();
						}
					}
				};
			}
		});
		try
		{
			final Future<V> future = pool.submit(callable);
			return future.get(timeoutSeconds, TimeUnit.SECONDS);
		}
		catch (final InterruptedException e)
		{
			Thread.currentThread().interrupt();
			fail("interrupted while waiting");
		}
		catch (final ExecutionException e)
		{
			fail("unexpected execution exception " + e.getCause());
		}
		catch (final TimeoutException e)
		{
			fail("callable " + callable + " did not finish within maximum " + timeoutSeconds + " seconds to wait");
		}
		finally
		{
			pool.shutdownNow();
		}
		return null;
	}


}
