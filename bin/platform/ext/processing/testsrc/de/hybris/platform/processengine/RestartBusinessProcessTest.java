/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.processengine;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.processengine.action.AbstractProceduralAction;
import de.hybris.platform.processengine.enums.ProcessState;
import de.hybris.platform.processengine.impl.BusinessProcessServiceDao;
import de.hybris.platform.processengine.impl.DefaultBusinessProcessService;
import de.hybris.platform.processengine.model.BusinessProcessModel;
import de.hybris.platform.processengine.model.ProcessTaskModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.task.RetryLaterException;
import de.hybris.platform.task.TaskService;
import de.hybris.platform.task.impl.DefaultBusinessProcessRestartStrategy;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.util.Collection;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


@IntegrationTest
public class RestartBusinessProcessTest extends ServicelayerBaseTest
{
	private static final String WAIT_NODE = "waitForever";
	private static final String START_NODE = "start";
	private static final String BEFORE_TEST = "beforeTest";
	private static final String PRE_WAIT_NODE = "beforeWait";
	private static final String EVENT_START_TEST = "_StartTest";
	private static final String EVENT_SOMETHING_TO_HAPPEN = "_SomethingToHappen";

	private static final long SLOW_ACTION_WAIT_MULTIPLIER = 4;
	private static final long WAIT_TIMEOUT = TimeUnit.SECONDS.toMillis(90);
	private final PropertyConfigSwitcher restartRetries = new PropertyConfigSwitcher(
			DefaultBusinessProcessRestartStrategy.PROCESSENGINE_PROCESS_RESTART_RETRIES);
	private final PropertyConfigSwitcher restartSeconds = new PropertyConfigSwitcher(
			DefaultBusinessProcessRestartStrategy.PROCESSENGINE_PROCESS_RESTART_MILLIS);
	private final PropertyConfigSwitcher restartException = new PropertyConfigSwitcher(
			DefaultBusinessProcessRestartStrategy.PROCESSENGINE_PROCESS_RESTART_EXCEPTION_IF_FAILED);
	private final PropertyConfigSwitcher restartLegacy = new PropertyConfigSwitcher(
			DefaultBusinessProcessService.PROCESSENGINE_PROCESS_RESTART_LEGACY);
	@Resource
	private BusinessProcessService businessProcessService;
	@Resource
	private TaskService taskService;
	@Resource
	private PreWaitAction preWaitAction;
	@Resource
	private PostWaitAction postWaitAction;
	@Resource
	private ModelService modelService;
	@Resource
	private BusinessProcessServiceDao businessProcessServiceDao;

	@After
	public void tearDown() throws Exception
	{
		preWaitAction.reset();
		postWaitAction.reset();
		restartRetries.switchBackToDefault();
		restartSeconds.switchBackToDefault();
		restartException.switchBackToDefault();
		restartLegacy.switchBackToDefault();
		taskService.getEngine().start();
	}

	/**
	 * HORST-1336 Repair business process duplicated TaskCondition problem
	 */
	@Test
	public void restartingProcessStoppedOnWaitNodeShouldNotFail() throws InterruptedException, TimeoutException
	{
		// given
		final String uuid = UUID.randomUUID().toString();
		final BusinessProcessModel processModel = businessProcessService.createProcess(uuid, "repairBusinessProcessDefinition");

		// when
		businessProcessService.startProcess(processModel);
		businessProcessService.triggerEvent(BusinessProcessEvent.newEvent(uuid + EVENT_START_TEST));
		waitForAction(processModel, WAIT_NODE);

		// then preWait action was invoked and we're at wait node
		assertThatActionsWereInvokedNTimes(1, 0);

		// and when
		businessProcessService.restartProcess(processModel, START_NODE);
		waitForAction(processModel, BEFORE_TEST);
		businessProcessService.triggerEvent(BusinessProcessEvent.newEvent(uuid + EVENT_START_TEST));
		waitForAction(processModel, WAIT_NODE);

		// then preWait action was once again invoked and we're at wait node
		assertThatActionsWereInvokedNTimes(2, 0);

		// and when
		businessProcessService.triggerEvent(BusinessProcessEvent.newEvent(uuid + EVENT_SOMETHING_TO_HAPPEN));
		waitForState(processModel, ProcessState.SUCCEEDED);

		// then postWait
		assertThatActionsWereInvokedNTimes(2, 1);
	}

	@Test
	public void restartingProcessDuringNormalSlowActionShouldThrowException() throws InterruptedException, TimeoutException
	{
		// given
		final Integer retries = 1;
		final Long millis = 500L;
		restartRetries.switchToValue(retries.toString());
		restartSeconds.switchToValue(millis.toString());
		restartException.switchToValue("true");
		preWaitAction.setExecuteSleepTime(SLOW_ACTION_WAIT_MULTIPLIER * retries * millis);

		final String uuid = UUID.randomUUID().toString();
		final BusinessProcessModel processModel = businessProcessService.createProcess(uuid, "repairBusinessProcessDefinition");

		// when
		businessProcessService.startProcess(processModel);
		businessProcessService.triggerEvent(BusinessProcessEvent.newEvent(uuid + EVENT_START_TEST));
		waitForAction(processModel, PRE_WAIT_NODE);
		waitForCondition(r -> preWaitAction.getInvocations() == 1L);

		// then
		assertThatThrownBy(() -> businessProcessService.restartProcess(processModel, START_NODE)).isInstanceOf(
				IllegalStateException.class).hasMessage("Couldn't restart business process " + processModel.getCode());

		businessProcessService.triggerEvent(BusinessProcessEvent.newEvent(uuid + EVENT_SOMETHING_TO_HAPPEN));
		waitForState(processModel, ProcessState.SUCCEEDED);

		assertThatActionsWereInvokedNTimes(1, 1);
	}

	@Test
	public void restartingProcessShouldRemoveAllUnlockedScheduledTasks() throws InterruptedException, TimeoutException
	{
		// given
		final Integer retries = 1;
		final Long millis = 500L;
		restartRetries.switchToValue(retries.toString());
		restartSeconds.switchToValue(millis.toString());
		restartException.switchToValue("false");
		preWaitAction.setExecuteSleepTime(SLOW_ACTION_WAIT_MULTIPLIER * retries * millis);

		final String uuid = UUID.randomUUID().toString();
		final BusinessProcessModel processModel = businessProcessService.createProcess(uuid, "repairBusinessProcessDefinition");

		// when
		taskService.getEngine().stop();
		businessProcessService.startProcess(processModel);

		final Collection<ProcessTaskModel> currentTasks = processModel.getCurrentTasks();
		assertThat(currentTasks).isNotEmpty();

		businessProcessService.restartProcess(processModel, START_NODE);

		//then
		assertThat(currentTasks.stream().allMatch(modelService::isRemoved)).isTrue();

		taskService.getEngine().start();
		businessProcessService.triggerEvent(BusinessProcessEvent.newEvent(uuid + EVENT_START_TEST));
		waitForAction(processModel, PRE_WAIT_NODE);
		waitForCondition(r -> preWaitAction.getInvocations() == 1L);

		businessProcessService.triggerEvent(BusinessProcessEvent.newEvent(uuid + EVENT_SOMETHING_TO_HAPPEN));
		waitForState(processModel, ProcessState.SUCCEEDED);

		assertThatActionsWereInvokedNTimes(1, 1);
	}

	@Test
	public void restartingProcessDuringNormalSlowActionShouldDoNothingIfExceptionDisabled()
			throws InterruptedException, TimeoutException
	{
		// given
		final Integer retries = 1;
		final Long millis = 500L;
		restartRetries.switchToValue(retries.toString());
		restartSeconds.switchToValue(millis.toString());
		restartException.switchToValue("false");
		preWaitAction.setExecuteSleepTime(SLOW_ACTION_WAIT_MULTIPLIER * retries * millis);

		final String uuid = UUID.randomUUID().toString();
		final BusinessProcessModel processModel = businessProcessService.createProcess(uuid, "repairBusinessProcessDefinition");

		// when
		businessProcessService.startProcess(processModel);
		businessProcessService.triggerEvent(BusinessProcessEvent.newEvent(uuid + EVENT_START_TEST));
		waitForAction(processModel, PRE_WAIT_NODE);
		waitForCondition(r -> preWaitAction.getInvocations() == 1L);

		businessProcessService.restartProcess(processModel, START_NODE);

		// then
		businessProcessService.triggerEvent(BusinessProcessEvent.newEvent(uuid + EVENT_SOMETHING_TO_HAPPEN));
		waitForState(processModel, ProcessState.SUCCEEDED);

		assertThatActionsWereInvokedNTimes(1, 1);
	}

	private void assertThatActionsWereInvokedNTimes(final int slowActionInvocations, final int afterWaitActionInvocations)
	{
		assertThat(preWaitAction.getInvocations()).isEqualTo(slowActionInvocations);
		assertThat(postWaitAction.getInvocations()).isEqualTo(afterWaitActionInvocations);
	}

	private void waitForAction(final BusinessProcessModel model, final String action)
			throws InterruptedException, TimeoutException
	{
		waitForCondition(r -> {

			try
			{
				final List<String> actions = businessProcessServiceDao.findBusinessProcessTaskActions(model.getPk());
				return actions.size() == 1 && actions.get(0).equals(action);
			}
			catch (final NoSuchElementException ex)
			{
				return Boolean.FALSE;
			}
		});
	}

	private void waitForState(final BusinessProcessModel model, final ProcessState state)
			throws InterruptedException, TimeoutException
	{
		waitForCondition(r -> {
			modelService.refresh(model);
			return model.getProcessState().equals(state);
		});
	}

	private void waitForCondition(final Function<Long, Boolean> condition) throws TimeoutException, InterruptedException
	{
		final long start = System.currentTimeMillis();
		long round = 1;
		while (!condition.apply(round))
		{
			if (System.currentTimeMillis() - start > WAIT_TIMEOUT)
			{
				throw new TimeoutException();
			}
			round++;
			Thread.sleep(Math.min(100, WAIT_TIMEOUT / 100));
		}
	}

	public static class PreWaitAction extends AbstractProceduralAction<BusinessProcessModel>
	{
		private static final Logger LOG = LoggerFactory.getLogger(PreWaitAction.class);
		private static final long DEFAULT_SLEEP_TIME_MILLIS = 500;

		private final AtomicLong invocations = new AtomicLong(0);
		private final AtomicLong sleepTimeMillis = new AtomicLong(DEFAULT_SLEEP_TIME_MILLIS);

		@Override
		public void executeAction(final BusinessProcessModel process) throws RetryLaterException, InterruptedException
		{
			final long l = invocations.incrementAndGet();
			Thread.sleep(sleepTimeMillis.get());
			LOG.info("PreWaitAction#{}, execute, invocations: {}", hashCode(), l);
		}

		public long getInvocations()
		{
			final long l = invocations.get();
			LOG.info("PreWaitAction#{}, getInvocations, invocations: {}", hashCode(), l);
			return l;
		}

		public void setExecuteSleepTime(final long millis)
		{
			sleepTimeMillis.set(millis);
		}

		public void reset()
		{
			invocations.set(0);
			sleepTimeMillis.set(DEFAULT_SLEEP_TIME_MILLIS);
		}
	}

	public static class PostWaitAction extends AbstractProceduralAction<BusinessProcessModel>
	{
		private static final Logger LOG = LoggerFactory.getLogger(PostWaitAction.class);
		private final AtomicLong invocations = new AtomicLong(0);

		@Override
		public void executeAction(final BusinessProcessModel process)
		{
			final long l = invocations.incrementAndGet();
			LOG.info("PostWaitAction#{}, execute, invocations: {}", hashCode(), l);
		}

		public long getInvocations()
		{
			final long l = invocations.get();
			LOG.info("PostWaitAction#{}, getInvocations, invocations: {}", hashCode(), l);
			return l;
		}

		public void reset()
		{
			invocations.set(0);
		}
	}

}
