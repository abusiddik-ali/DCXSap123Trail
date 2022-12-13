/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.commons.renderer.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.util.VelocityHelper;

import java.io.StringWriter;

import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class VelocityEngineTest extends ServicelayerBaseTest
{

	@Before
	public void setUp() throws Exception
	{
		VelocityHelper.init();
	}

	/**
	 * Test resolving macros in string literal
	 * https://issues.apache.org/jira/browse/VELOCITY-944
	 * https://cxjira.sap.com/browse/HORST-7315
	 */
	@Test
	public void testMacroInStringLiteral()
	{
		final String template = "#macro( m $v )\n" +
				"<span>$v</span>\n" +
				"#end\n" +
				"#set($v = \"#m('bar')\")\n" +
				"$v\n" +
				"#m( 'foo' )";

		final String expected = "<span>bar</span>\n" +
				"<span>foo</span>";

		final VelocityContext ctx = new VelocityContext();
		final StringWriter resultWriter = new StringWriter();
		Velocity.evaluate(ctx, resultWriter, "", template);

		assertThat(resultWriter.toString()).isEqualToIgnoringWhitespace(expected);
	}
}
