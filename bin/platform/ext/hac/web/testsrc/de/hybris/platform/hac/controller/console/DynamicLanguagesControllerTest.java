/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.hac.controller.console;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.hac.facade.HacDynamicLanguagesFacade;
import de.hybris.platform.scripting.enums.ScriptType;
import de.hybris.platform.scripting.model.ScriptModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.annotation.Resource;

import org.apache.commons.lang3.RandomStringUtils;
import org.junit.Before;
import org.junit.Test;

import com.jayway.jsonpath.JsonPath;

import net.minidev.json.JSONArray;
import net.minidev.json.parser.JSONParser;
import net.minidev.json.parser.ParseException;

@IntegrationTest
public class DynamicLanguagesControllerTest extends ServicelayerBaseTest
{
	@Resource
	private HacDynamicLanguagesFacade hacDynamicLanguagesFacade;
	@Resource
	private ModelService modelService;

	private DynamicLanguagesController dynamicLanguagesController;

	@Before
	public void setUp()
	{
		dynamicLanguagesController = new DynamicLanguagesController(hacDynamicLanguagesFacade);
	}

	@Test
	public void shouldReturnProperJSONWithAllScripts() throws ParseException
	{
		final Set<String> codes = new HashSet<>();
		codes.add("a\"}]}]}];alert(1); var a = [{1:[{1:[{name:\"a");
		codes.add(RandomStringUtils.randomAlphanumeric(15));

		codes.forEach(this::createGroovyScript);

		final String jsonString = dynamicLanguagesController.getAllScripts();
		new JSONParser(JSONParser.MODE_STRICTEST).parse(jsonString, JSONArray.class);

		System.out.println(jsonString);
		final List<String> names = JsonPath.read(jsonString, "$.*.children.*.children.*.name");
		assertThat(names).containsAll(codes);
	}

	private void createGroovyScript(final String code)
	{
		final ScriptModel script = modelService.create(ScriptModel.class);
		script.setCode(code);
		script.setContent("print 'hey!!!'");
		script.setScriptType(ScriptType.GROOVY);
		modelService.save(script);
	}


}
