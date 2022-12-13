/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.persistence;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.persistence.hjmp.HJMPUtils;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.test.TestThreadsHolder;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.testframework.TestModelUtils;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;

public abstract class AbstractOptimisticLockingIntegrationTest extends ServicelayerBaseTest
{
	public static final int TIMEOUT_IN_SECONDS_FOR_CONCURRENT_THREADS = 30;
	public static final int NUMBER_OF_THREADS = 10;
	public static final int INCREMENT_COUNT = 5;
	public static final int INTEGER_ATTR_VALUE = NUMBER_OF_THREADS * INCREMENT_COUNT;
	protected final PropertyConfigSwitcher persistenceLegacyModeSwitch = new PropertyConfigSwitcher("persistence.legacy.mode");
	protected final PropertyConfigSwitcher optimisticLockSwitch = new PropertyConfigSwitcher(
			"hjmp.throw.concurrent.modification.exceptions");
	@Resource
	protected ModelService modelService;

	protected ItemModel testModel1;
	protected ItemModel testModel2;

	@Before
	public void setUp()
	{
		testModel1 = createTestModelWithCodeAndIntegerAttrProperty("fooBar1");
		testModel2 = createTestModelWithCodeAndIntegerAttrProperty("fooBar2");
	}

	@After
	public void tearDown()
	{
		optimisticLockSwitch.switchBackToDefault();
		persistenceLegacyModeSwitch.switchBackToDefault();
	}

	protected abstract ItemModel createTestModelWithCodeAndIntegerAttrProperty(final String code);

	protected void updateAndAssertItemInManyThreadsRetryingOperationOnConcurrentModificationException(
			final boolean withThreadSetting, final PK pk)
	{
		final Runnable runnable = createIncrementRunnable(withThreadSetting, pk);
		final TestThreadsHolder<Runnable> holder = new TestThreadsHolder<>(NUMBER_OF_THREADS, runnable, true);

		// when
		holder.startAll();
		final boolean allThreadsFinished = holder.waitAndDestroy(getTestTimeout());

		// then
		assertThat(allThreadsFinished).as("Not all increment threads finished").isTrue();
		assertThat(holder.getErrors()).overridingErrorMessage("Errors occurred during thread execution " + holder.getErrors())
		                              .isEmpty();
		testModel1 = TestModelUtils.reReadModel(testModel1);
		verifyResult(testModel1);
	}

	protected long getTestTimeout()
	{
		return TIMEOUT_IN_SECONDS_FOR_CONCURRENT_THREADS;
	}

	protected abstract void verifyResult(final ItemModel testModel);

	private Runnable createIncrementRunnable(final boolean withThreadSetting, final PK pk)
	{
		if (withThreadSetting)
		{
			return new IncrementCounterRunnerWithOptimisticLockingSetting(pk);
		}
		else
		{
			return new IncrementCounterRunner(pk);
		}
	}

	protected abstract void runTestService(PK testModelPk);

	class IncrementCounterRunnerWithOptimisticLockingSetting extends IncrementCounterRunner
	{


		IncrementCounterRunnerWithOptimisticLockingSetting(final PK testModelPk)
		{
			super(testModelPk);
		}

		@Override
		public void run()
		{
			try
			{
				HJMPUtils.enableOptimisticLocking();
				super.run();
			}
			finally
			{
				HJMPUtils.clearOptimisticLockingSetting();
			}
		}
	}

	class IncrementCounterRunner implements Runnable
	{
		private final PK testModelPk;

		IncrementCounterRunner(final PK testModelPk)
		{
			this.testModelPk = testModelPk;
		}


		@Override
		public void run()
		{
			for (int i = 0; i < AbstractOptimisticLockingIntegrationTest.INCREMENT_COUNT; i++)
			{
				runTestService(testModelPk);
			}
		}
	}
}
