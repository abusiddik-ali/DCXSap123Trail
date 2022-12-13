/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.security;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import org.assertj.core.api.Assertions;
import org.junit.Test;

/**
 * This test class has been created to make sure there is no {@code org.apache.log4j.net.SocketServer} available on classpath.
 * This class is known to have a vulnerability: CVE-2019-17571 https://nvd.nist.gov/vuln/detail/CVE-2019-17571
 */
@IntegrationTest
public class Log4JSocketServletTest extends ServicelayerBaseTest
{

	@Test
	public void shouldNotContainSocketServerInClasspath()
	{
		Assertions.assertThatThrownBy(() -> Class.forName("org.apache.log4j.net.SocketServer"))
		          .isInstanceOf(ClassNotFoundException.class);
	}
}
