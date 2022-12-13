/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.testframework.HybrisJUnit4Test;
import de.hybris.platform.util.Config;
import de.hybris.platform.util.MediaUtil;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class MediaUtilIntegrationTest extends HybrisJUnit4Test
{

	private String mediaWebrootBackup;


	@Before
	public void setUp() throws Exception
	{
		mediaWebrootBackup = Config.getParameter("mediaweb.webroot");
	}

	@After
	public void tearDown()
	{
		Config.setParameter("mediaweb.webroot", mediaWebrootBackup);
	}

	@Test
	public void shouldReturnLocalMediaWebRootUrl()
	{
		// given
		Config.setParameter("mediaweb.webroot", null);

		// when
		final String rootURL = MediaUtil.getLocalMediaWebRootUrl();

		// then
		assertThat(rootURL).isNotEmpty().isEqualTo("/medias");
	}
}
