/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.processengine.definition;


import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.processengine.definition.EndNode.Type;
import de.hybris.platform.processengine.model.BusinessProcessModel;
import de.hybris.platform.scripting.engine.ScriptingLanguagesService;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import java.net.URL;
import java.util.Collections;

import org.apache.log4j.Logger;
import org.assertj.core.api.Assertions;
import org.junit.Before;
import org.junit.Test;
import org.springframework.context.support.GenericApplicationContext;


@IntegrationTest
public class ProcessDefinitionFactoryTest extends ServicelayerBaseTest
{
	private static final Logger LOG = Logger.getLogger(ProcessDefinitionFactoryTest.class.getName());
	private DefaultProcessDefinitionFactory processDefinitionFactoryStub;

	@Before
	public void setUp()
	{
		final GenericApplicationContext applicationContext = new GenericApplicationContext();

		final XMLProcessDefinitionsReader xmlDefinitionsReader = new XMLProcessDefinitionsReader(Registry.getApplicationContext()
		                                                                                                 .getBean(
				                                                                                                 "scriptingLanguagesService",
				                                                                                                 ScriptingLanguagesService.class));

		final ProcessDefinitionsProvider processDefinitionsProvider = new TestProcessDefinitionsProvider(xmlDefinitionsReader);
		processDefinitionsProvider.setApplicationContext(applicationContext);

		final ProcessDefinitionsCache definitionsCache = new ProcessDefinitionsCache();
		definitionsCache.setProcessDefinitionsProvider(processDefinitionsProvider);

		processDefinitionFactoryStub = new DefaultProcessDefinitionFactory();
		processDefinitionFactoryStub.setApplicationContext(applicationContext);
		processDefinitionFactoryStub.setDefinitionsCache(definitionsCache);
		processDefinitionFactoryStub.setXmlDefinitionsReader(xmlDefinitionsReader);

		applicationContext.refresh();
	}

	@Test
	public void testProcessDefinitionFactoryGetAllProcessDefinitions()
	{
		final String name1 = "name1";
		final String name2 = "name2";
		final String name3 = "name3";
		final Node node = new EndNode("1", Type.SUCCEEDED, "");
		processDefinitionFactoryStub.add(new ProcessDefinition(name1, node, null, Collections.singletonMap("1", node), Collections
				.singletonMap("1", (ContextParameterDeclaration) null), BusinessProcessModel.class.toString(), null));
		processDefinitionFactoryStub.add(new ProcessDefinition(name2, node, null, Collections.singletonMap("1", node), Collections
				.singletonMap("1", (ContextParameterDeclaration) null), BusinessProcessModel.class.toString(), null));
		processDefinitionFactoryStub.add(new ProcessDefinition(name3, node, null, Collections.singletonMap("1", node), Collections
				.singletonMap("1", (ContextParameterDeclaration) null), BusinessProcessModel.class.toString(), null));

		Assertions.assertThat(processDefinitionFactoryStub.getAllProcessDefinitionsNames()).containsOnly(name1, name2, name3);
	}

	private void reject(final String resourceName)
	{
		this.reject(this.getClass().getResource(resourceName));
	}

	private void reject(final URL url)
	{
		try
		{
			this.processDefinitionFactoryStub.add(url);
			fail("Invalid process definition was accepted.");
		}
		catch (final InvalidProcessDefinitionException e)
		{
			LOG.info("Correctly rejected invalid process definition: " + e.getMessage());
		}
	}

	private void accept(final String resourceName)
	{
		this.accept(this.getClass().getResource(resourceName));
	}

	private void accept(final URL url)
	{
		final ProcessDefinitionId id = new ProcessDefinitionId(processDefinitionFactoryStub.add(url));
		assertNotNull("defFac.getProcessDefinition", this.processDefinitionFactoryStub.getProcessDefinition(id));
	}

	@Test
	public void testValid() throws Exception
	{
		this.accept("/processengine/test/valid.xml");
	}

	@Test
	public void testNoActions() throws Exception
	{
		this.reject("/processengine/test/noActions.xml");
	}

	@Test
	public void testInvalidNamespace() throws Exception
	{
		this.reject("/processengine/test/missingStartAction.xml");
	}

	@Test
	public void testMissingStartAction() throws Exception
	{
		this.reject("/processengine/test/missingStartAction.xml");
	}

	@Test
	public void testInvalidStartAction() throws Exception
	{
		this.reject("/processengine/test/invalidStartAction.xml");
	}

	@Test
	public void testEmptyStartAction() throws Exception
	{
		this.reject("/processengine/test/emptyStartAction.xml");
	}

	@Test
	public void testMissingProcessName() throws Exception
	{
		this.reject("/processengine/test/missingProcessName.xml");
	}

	@Test
	public void testEmptyProcessName() throws Exception
	{
		this.reject("/processengine/test/emptyProcessName.xml");
	}

	@Test
	public void testDuplicateContextParameter() throws Exception
	{
		this.reject("/processengine/test/duplicateContextParameter.xml");
	}


	@Test
	public void testMissingContextParameterName() throws Exception
	{
		this.reject("/processengine/test/missingContextParameterName.xml");
	}

	@Test
	public void testEmptyContextParameterName() throws Exception
	{
		this.reject("/processengine/test/emptyContextParameterName.xml");
	}


	@Test
	public void testMissingContextParameterType() throws Exception
	{
		this.reject("/processengine/test/missingContextParameterType.xml");
	}

	@Test
	public void testEmptyContextParameterType() throws Exception
	{
		this.reject("/processengine/test/emptyContextParameterType.xml");
	}

	@Test
	public void testInvalidContextParameterType() throws Exception
	{
		this.reject("/processengine/test/invalidContextParameterType1.xml");
		this.reject("/processengine/test/invalidContextParameterType2.xml");
		this.reject("/processengine/test/invalidContextParameterType3.xml");
	}


	@Test
	public void testEmptyOnErrorAction() throws Exception
	{
		this.reject("/processengine/test/emptyOnErrorAction.xml");
	}

	@Test
	public void testInvalidOnErrorAction() throws Exception
	{
		this.reject("/processengine/test/invalidOnErrorAction.xml");
	}
}
