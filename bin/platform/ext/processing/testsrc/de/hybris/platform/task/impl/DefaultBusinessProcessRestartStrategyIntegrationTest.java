package de.hybris.platform.task.impl;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertTrue;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.processengine.model.BusinessProcessModel;
import de.hybris.platform.processengine.model.ProcessTaskModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.task.TaskConditionModel;
import de.hybris.platform.task.TaskService;

import java.util.Collections;
import java.util.UUID;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Test;

@IntegrationTest
public class DefaultBusinessProcessRestartStrategyIntegrationTest extends ServicelayerBaseTest
{
	@Resource
	ModelService modelService;

	@Resource
	TaskService taskService;

	@Resource
	TaskDAO taskDao;


	@After
	public void tearDown()
	{
		taskService.getEngine().start();
	}

	@Test
	public void testRemoveUnlockedTasks() throws Exception
	{
		final String uuid = UUID.randomUUID().toString();

		assertTrue("task engine is not running", taskService.getEngine().isRunning());
		final BusinessProcessModel process = modelService.create(BusinessProcessModel.class);
		process.setCode(uuid);
		process.setProcessDefinitionName("repairBusinessProcessDefinition");
		modelService.save(process);

		final ProcessTaskModel processTaskModel = modelService.create(ProcessTaskModel.class);
		processTaskModel.setAction("beforeTest");
		processTaskModel.setProcess(process);
		processTaskModel.setRunnerBean("testTaskRunner");

		final TaskConditionModel condition = modelService.create(TaskConditionModel.class);
		condition.setUniqueID(uuid + "_StartTest");
		processTaskModel.setConditions(Collections.singleton(condition));


		taskService.getEngine().stop();
		taskService.scheduleTask(processTaskModel);
		modelService.refresh(processTaskModel);

		assertNotNull(processTaskModel.getPk());
		assertEquals(-1, (int) processTaskModel.getRunningOnClusterNode());

		DefaultBusinessProcessRestartStrategy restartStrategy = new DefaultBusinessProcessRestartStrategy(taskDao, modelService);
		restartStrategy.makeARequestToRestartProcess(process);

		taskService.getEngine().start();
		assertTrue(modelService.isRemoved(processTaskModel));
	}
}