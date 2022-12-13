/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.scripting.engine.content.impl;

import static junit.framework.Assert.fail;
import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.scripting.engine.exception.ScriptURIException;

import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.springframework.core.io.ClassPathResource;


public class ResourceScriptContentTest
{
	@Test
	public void shouldCreateClassPathScriptContentObject() throws Exception
	{
		// given
		final ClassPathResource resource = new ClassPathResource("test/test-script.groovy");
		final ResourceScriptContent scriptContent = new ResourceScriptContent(resource);

		// when
		final String content = scriptContent.getContent();
		final String engineName = scriptContent.getEngineName();
		final Map<String, Object> customContext = scriptContent.getCustomContext();

		// then
		assertThat(content.replaceFirst("/\\*(.|[\\r\\n])*\\*/\\n", StringUtils.EMPTY))
				.isEqualTo("def names = ['John', 'Richard', \"Peter\"]\nnames.sort().join(',')");
		assertThat(engineName).isEqualTo("groovy");
		assertThat(customContext).isEmpty();
	}

	@Test
	public void shouldThrowScriptURIExceptionWhenFileExtensionFromResourceCannotBeDetermined() throws Exception
	{
		// given
		final ClassPathResource resource = new ClassPathResource("test/test-script");

		try
		{
			// when
			new ResourceScriptContent(resource);
			fail("should throw ScriptURIException");
		}
		catch (final ScriptURIException e)
		{
			// then fine
		}

	}
}
