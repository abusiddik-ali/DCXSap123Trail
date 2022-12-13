/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.commons.jalo;


import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.jalo.media.Media;
import de.hybris.platform.jalo.media.MediaManager;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.junit.Test;


@IntegrationTest
public class VelocityFormatterTest extends ServicelayerBaseTest
{
	private static final String TEMPLATE_VM = "template.vm";
	private static final String TEST_VELOCITY_FORMATTER = "testVelocityFormatter";

	@Test
	public void shouldNotAllowJavaReflectionAPICalls() throws IOException
	{
		// given
		final VelocityFormatter formatter = createVelocityFormatter();

		// when
		// formatter is used also as content provider for velocity template
		final Media ret = formatter.format(formatter);
		final String line = FileUtils.readFileToString(ret.getFile());

		// then
		assertThat(line).isEqualTo("${systemClass.exit(1)}\nThis is " + TEST_VELOCITY_FORMATTER);
	}

	private VelocityFormatter createVelocityFormatter()
	{
		final Map<String, String> attributes = new HashMap<>();
		attributes.put(VelocityFormatter.CODE, TEST_VELOCITY_FORMATTER);
		attributes.put(VelocityFormatter.LOCATION, createVelocityTemplateMedia().getLocation());

		return CommonsManager.getInstance().createVelocityFormatter(attributes);
	}

	private Media createVelocityTemplateMedia()
	{
		final Media media = MediaManager.getInstance().createMedia(TEMPLATE_VM);
		media.setData(new ByteArrayInputStream(("#set( $str = \"\" )\n#set( $systemClass = ${str.getClass().forName( \"java.lang.System\" )} )\n${systemClass.exit(1)}\n"
				+ "This is $this.code").getBytes()), TEMPLATE_VM, "text/x-velocity");
		return media;
	}
}
