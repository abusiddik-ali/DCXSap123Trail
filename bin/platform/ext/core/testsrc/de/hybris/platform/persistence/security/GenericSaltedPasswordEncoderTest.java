/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.security;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.testframework.HybrisJUnit4Test;

import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class GenericSaltedPasswordEncoderTest extends HybrisJUnit4Test
{

	private PasswordEncoderFactory passwordEncoderFactory;

	@Before
	public void prepareTest()
	{
		passwordEncoderFactory = Registry.getApplicationContext().getBean("core.passwordEncoderFactory",
				PasswordEncoderFactory.class);
	}

	@Test(expected = RuntimeException.class)
	public void nonExistingAlgorithmShouldThrowException()
	{
		DigestCalculator.getInstance("noneSuch");
	}

	@Test
	public void testDigestCalculator()
	{
		final PasswordEncoder deprecatedEncoder = passwordEncoderFactory.getEncoder("md5");
		final PasswordEncoder genericEncoder = Registry.getApplicationContext()
		                                               .getBean("md5PasswordEncoder", PasswordEncoder.class);

		assertThat(deprecatedEncoder.encode("uid", "foo")).isEqualTo(genericEncoder.encode("uid", "foo"));
	}
}
