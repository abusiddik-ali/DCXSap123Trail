/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.processengine.synchronus;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertNotNull;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.processengine.BusinessProcessService;
import de.hybris.platform.processengine.action.AbstractProceduralAction;
import de.hybris.platform.processengine.action.AbstractSimpleDecisionAction;
import de.hybris.platform.processengine.definition.ProcessDefinitionFactory;
import de.hybris.platform.processengine.model.BusinessProcessModel;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
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
public class SynchLoopOneActionFromAnotherActionTest extends AbstractProcessEngineIntegrationTest
{

	protected final PropertyConfigSwitcher breakWhenLoop = new PropertyConfigSwitcher(
			"processengine.process.canjoinpreviousnode.break.when.loop");

	public static final String TEST_PROCESS = "test-proc";
	public static final String TEST_PROCESS2 = "test-proc2";
	public static final String PROCESS_DEFINITION_NAME = "synchBreak";
	public static final String PROCESS_DEFINITION_NAME_TWO_LOOPS = "synchTwoLoops";
	private static String UID1;
	private static String UID2;

	private final ClassPathResource processDefinition = new ClassPathResource("processengine/test/synch_loop.xml");
	private final ClassPathResource processDefinitionTwoLoops = new ClassPathResource("processengine/test/synch_two_loops.xml");

	@Resource
	private BusinessProcessService businessProcessService;

	@Resource
	private ModelService modelService;

	@Resource
	private ProcessDefinitionFactory processDefinitionFactory;

	@Resource
	private UserService userService;

	private CheckIfCreateUserAction testActionBean;
	private CreateUserAction createUserActionBean;

	private DefaultListableBeanFactory beanFactory;

	public static final String CHECK_IF_CREATE_USER_ACTION_BEAN = "CheckIfCreateUserActionBean";
	public static final String CREATE_USER_ACTION_BEAN = "CreateUserAction";
	private SynchroAsynchroTracker tracker;
	private static Context context;


	@Before
	public void doBefore() throws IOException
	{
		canJoinPreviousNodeDefaultValue.switchToValue("false");
		context = new Context();

		final ConfigurableApplicationContext appCtx = (ConfigurableApplicationContext) Registry.getApplicationContext();
		beanFactory = (DefaultListableBeanFactory) appCtx.getBeanFactory();
		testActionBean = new CheckIfCreateUserAction(userService);
		beanFactory.registerSingleton(CHECK_IF_CREATE_USER_ACTION_BEAN, testActionBean);
		createUserActionBean = new CreateUserAction(modelService);
		beanFactory.registerSingleton(CREATE_USER_ACTION_BEAN, createUserActionBean);

		processDefinitionFactory.add(processDefinition);
		BusinessProcessModel bpm = modelService.create(BusinessProcessModel.class);
		bpm.setCode(TEST_PROCESS);
		bpm.setProcessDefinitionName(PROCESS_DEFINITION_NAME);

		processDefinitionFactory.add(processDefinitionTwoLoops);
		bpm = modelService.create(BusinessProcessModel.class);
		bpm.setCode(TEST_PROCESS2);
		bpm.setProcessDefinitionName(PROCESS_DEFINITION_NAME_TWO_LOOPS);
		modelService.saveAll();
	}


	@After
	public void doAfter()
	{
		canJoinPreviousNodeDefaultValue.switchBackToDefault();
		beanFactory.destroySingleton(CHECK_IF_CREATE_USER_ACTION_BEAN);
		beanFactory.destroySingleton(CREATE_USER_ACTION_BEAN);
	}


	@Test
	public void shouldNotRunSeparateTaskWhenLongLoopDetected() throws TimeoutException
	{
		breakWhenLoop.switchToValue("false");
		try
		{
			UID1 = TEST_PROCESS + "test1_" + new Random(1000).nextInt();
			//given
			tracker = new SynchroAsynchroTracker(true, false, true, true, true);
			final BusinessProcessModel bpm = businessProcessService.getProcess(TEST_PROCESS);

			// when
			businessProcessService.startProcess(bpm);
			waitForBusinessProcess(bpm);

			//then
			assertThat(tracker.isTrackExecuteCorrectly()).isTrue();
		}
		finally
		{
			breakWhenLoop.switchBackToDefault();
		}
	}


	@Test
	public void shouldNotRunSeparateTasksWhenLongLoopsDetected() throws TimeoutException
	{
		breakWhenLoop.switchToValue("false");
		try
		{
			UID1 = TEST_PROCESS2 + "test1_" + new Random(1000).nextInt();
			UID2 = TEST_PROCESS2 + "test2_" + new Random(1000).nextInt();
			//given
			tracker = new SynchroAsynchroTracker(true, false, true, true, true, true, true, true, true);
			final BusinessProcessModel bpm = businessProcessService.getProcess(TEST_PROCESS2);

			// when
			businessProcessService.startProcess(bpm);
			waitForBusinessProcess(bpm);

			//then
			assertThat(tracker.isTrackExecuteCorrectly()).isTrue();
		}
		finally
		{
			breakWhenLoop.switchBackToDefault();
		}

	}

	private static class CheckIfCreateUserAction extends AbstractSimpleDecisionAction<BusinessProcessModel>
	{

		final UserService userService;

		public CheckIfCreateUserAction(final UserService userService)
		{
			this.userService = userService;
		}

		public Transition executeAction(final BusinessProcessModel process)
		{
			try
			{
				final String actionName = process.getCurrentTasks().iterator().next().getAction();
				final String userName = actionName.contains("Second") ? UID2 : UID1;
				getContext().action(actionName);
				final UserModel uModel = userService.getUserForUID(userName);
				return Transition.OK;
			}
			catch (final UnknownIdentifierException uie)
			{
				return Transition.NOK;
			}
		}

	}

	private static class CreateUserAction extends AbstractProceduralAction
	{

		final ModelService modelService;

		public CreateUserAction(final ModelService modelService)
		{
			this.modelService = modelService;
		}

		@Override
		public void executeAction(final BusinessProcessModel process) throws Exception
		{
			final String actionName = process.getCurrentTasks().iterator().next().getAction();
			getContext().action(actionName);
			final String userName = actionName.contains("Second") ? UID2 : UID1;
			createUser(userName);
		}

		private UserModel createUser(final String uid)
		{
			final UserModel user = modelService.create(UserModel.class);
			user.setUid(uid);
			modelService.save(user);
			assertNotNull(user);
			return user;
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


	public static class SynchroAsynchroTracker
	{
		String currentThreadName;
		int currentExecutionNumber = 0;
		List<Boolean> syncActions;
		boolean trackExecuteCorrectly = true;

		public boolean isTrackExecuteCorrectly()
		{
			return trackExecuteCorrectly;
		}

		public SynchroAsynchroTracker(final Boolean... track)
		{
			syncActions = Arrays.asList(track);
		}

		public void trackThreadAndTransactionForAction(final String threadName, final String actionName)
		{
			if (currentThreadName == null)
			{
				currentThreadName = threadName;
			}

			final Boolean syncAction = syncActions.get(currentExecutionNumber);
			if (syncAction)
			{
				if (!threadName.equals(currentThreadName))
				{
					trackExecuteCorrectly = false;
				}
			}
			if (!syncAction)
			{
				if (threadName.equals(currentThreadName))
				{
					trackExecuteCorrectly = false;
				}
			}
			if (!trackExecuteCorrectly)
			{
				System.out.println("Track is not correct for " + actionName + " sync: " + syncAction);
			}

			currentThreadName = threadName;
			currentExecutionNumber++;
		}
	}

}

