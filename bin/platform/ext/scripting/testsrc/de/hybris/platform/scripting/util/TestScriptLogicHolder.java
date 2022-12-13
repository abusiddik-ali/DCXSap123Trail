/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.scripting.util;

import de.hybris.platform.scripting.model.ScriptModel;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class TestScriptLogicHolder
{
	private final static ConcurrentMap<String, Logic> scriptLogics = new ConcurrentHashMap<>();
	private static final int REGISTER_SCRIPT_MAX_TRIES = 10;

	private final Set<String> instanceScripts = new HashSet<>();

	public TestScriptLogicHolder()
	{
	}

	public static <T extends Logic> T getScriptLogic(final String id, final Class<T> clazz)
	{
		return clazz.cast(scriptLogics.get(id));
	}


	public ScriptModel scriptWithLogic(final ModelService modelService, final TestScriptLogic scriptLogic)
	{
		return scriptWithLogic(modelService, UUID.randomUUID().toString(), scriptLogic);
	}

	public ScriptModel scriptWithLogic(final ModelService modelService, final String scriptCode,
	                                   final TestScriptLogic scriptLogic)
	{
		return createScriptModel(modelService, scriptCode, scriptLogic, false);
	}

	public ScriptModel scriptWithLogic(final ModelService modelService, final TestScriptLogicWithResult<?> scriptLogic)
	{
		return scriptWithLogic(modelService, UUID.randomUUID().toString(), scriptLogic);
	}

	public ScriptModel scriptWithLogic(final ModelService modelService, final String scriptCode,
	                                   final TestScriptLogicWithResult<?> scriptLogic)
	{
		return createScriptModel(modelService, scriptCode, scriptLogic, true);
	}

	private ScriptModel createScriptModel(final ModelService modelService, final String scriptCode,
	                                      final Logic scriptLogic, final boolean withResult)
	{
		final String scriptLogicId = registerScript(Objects.requireNonNull(scriptLogic));

		final ScriptModel script = modelService.create(ScriptModel.class);
		script.setCode(scriptCode);
		final String content;
		if (withResult)
		{
			content = createScriptContentWithResult(scriptLogicId);
		}
		else
		{
			content = createScriptContent(scriptLogicId);
		}
		script.setContent(content);
		modelService.saveAll(script);

		return script;
	}

	private String registerScript(final Logic scriptLogic)
	{
		String scriptLogicId = null;
		int tries = REGISTER_SCRIPT_MAX_TRIES;
		boolean inserted = false;
		while (!inserted && tries-- > 0)
		{
			scriptLogicId = UUID.randomUUID().toString();
			if (scriptLogics.putIfAbsent(scriptLogicId, scriptLogic) == null)
			{
				inserted = true;
			}
		}

		if (!inserted)
		{
			throw new IllegalStateException(
					"could not register the script logic (tried " + REGISTER_SCRIPT_MAX_TRIES + " times)!");
		}

		scriptLogics.put(scriptLogicId, scriptLogic);
		instanceScripts.add(scriptLogicId);
		return scriptLogicId;
	}

	private String createScriptContent(final String scriptLogicId)
	{
		//language=Groovy
		return "import "+TestScriptLogicHolder.class.getName()+"."+TestScriptLogic.class.getSimpleName()+"\n" +
				"import static "+TestScriptLogicHolder.class.getName()+".getScriptLogic\n" +
				"\n" +
				"def scriptLogicId = '" + scriptLogicId + "'\n" +
				"def scriptLogic = getScriptLogic(scriptLogicId, TestScriptLogic.class)\n" +
				"scriptLogic.run()";
	}


	private String createScriptContentWithResult(final String scriptLogicId)
	{
		//language=Groovy
		return "import "+TestScriptLogicHolder.class.getName()+"."+TestScriptLogicWithResult.class.getSimpleName()+"\n" +
				"\n" +
				"import static "+TestScriptLogicHolder.class.getName()+".getScriptLogic\n" +
				"\n" +
				"def scriptLogicId = '" + scriptLogicId + "'\n" +
				"def scriptLogic = getScriptLogic(scriptLogicId, TestScriptLogicWithResult.class)\n" +
				"def result = scriptLogic.run()\n" +
				"result";
	}

	public void clear()
	{
		instanceScripts.forEach(scriptLogics::remove);
		instanceScripts.clear();
	}

	public interface Logic
	{
	}

	public interface TestScriptLogic extends Logic
	{
		void run() throws Exception;
	}

	public interface TestScriptLogicWithResult<T> extends Logic
	{
		T run() throws Exception;
	}
}
