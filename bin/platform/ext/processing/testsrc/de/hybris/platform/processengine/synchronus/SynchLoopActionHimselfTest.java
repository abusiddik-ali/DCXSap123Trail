/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.processengine.synchronus;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertNotNull;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.processengine.action.AbstractSimpleDecisionAction;
import de.hybris.platform.processengine.definition.ProcessDefinitionFactory;
import de.hybris.platform.processengine.model.BusinessProcessModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.concurrent.TimeoutException;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.io.ClassPathResource;

@IntegrationTest
public class SynchLoopActionHimselfTest extends AbstractProcessEngineIntegrationTest
{

	public static final String TEST_PROCESS = "test-proc";
	public static final String PROCESS_DEFINITION_NAME = "synchActionLoopToHimself";
	private static final String ACTION_NAME = "loopAction";

	private final ClassPathResource processDefinition = new ClassPathResource("processengine/test/synchActionLoopToHimself.xml");

	@Resource
	private BusinessProcessService businessProcessService;

	@Resource
	private ModelService modelService;

	@Resource
	private ProcessDefinitionFactory processDefinitionFactory;

	@Resource
	private UserService userService;

	private ExecuteThreeTimesAction testActionBean;
	private DefaultListableBeanFactory beanFactory;
	public static final String EXECUTE_THREE_TIMES_ACTION = "ExecuteThreeTimesAction";

	private SynchLoopOneActionFromAnotherActionTest.SynchroAsynchroTracker tracker;
	private static Context context;


	@Before
	public void doBefore() throws IOException
	{
		canJoinPreviousNodeDefaultValue.switchToValue("true");
		context = new Context();

		final ConfigurableApplicationContext appCtx = (ConfigurableApplicationContext) Registry.getApplicationContext();
		beanFactory = (DefaultListableBeanFactory) appCtx.getBeanFactory();
		testActionBean = new ExecuteThreeTimesAction(userService);
		beanFactory.registerSingleton(EXECUTE_THREE_TIMES_ACTION, testActionBean);

		processDefinitionFactory.add(processDefinition);
		final BusinessProcessModel bpm = modelService.create(BusinessProcessModel.class);
		bpm.setCode(TEST_PROCESS);
		bpm.setProcessDefinitionName(PROCESS_DEFINITION_NAME);


		modelService.saveAll();
	}


	@After
	public void doAfter()
	{
		canJoinPreviousNodeDefaultValue.switchBackToDefault();
		beanFactory.destroySingleton(EXECUTE_THREE_TIMES_ACTION);
	}


	@Test
	public void shouldRunSeparateTaskWhenLoopDetected() throws TimeoutException
	{
		//given
		tracker = new SynchLoopOneActionFromAnotherActionTest.SynchroAsynchroTracker(true, true, false, false, false, true);
		final BusinessProcessModel bpm = businessProcessService.getProcess(TEST_PROCESS);

		// when
		businessProcessService.startProcess(bpm);
		waitForBusinessProcess(bpm);

		//then
		assertThat(tracker.isTrackExecuteCorrectly()).isTrue();
	}

	private static class ExecuteThreeTimesAction extends AbstractSimpleDecisionAction<BusinessProcessModel>
	{

		final UserService userService;

		private int executionNumber = 0;

		public ExecuteThreeTimesAction(final UserService userService)
		{
			this.userService = userService;
		}

		public Transition executeAction(final BusinessProcessModel process)
		{
			getContext().action(ACTION_NAME);

			if (executionNumber++ < 3)
			{
				return Transition.NOK;
			}
			else
			{
				return Transition.OK;
			}
		}
	}

	public static Context getContext()
	{
		return context;
	}

	private class Context
	{
		public void action(final String action)
		{
			tracker.trackThreadAndTransactionForAction(Thread.currentThread().getName(), action);
		}
	}

}

