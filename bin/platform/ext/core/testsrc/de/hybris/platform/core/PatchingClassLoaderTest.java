/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.bootstrap.loader.PlatformInPlaceClassLoader;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.Ignore;
import org.junit.Test;

@Ignore //if the patched version is needed - it should be unignored again
@IntegrationTest
public class PatchingClassLoaderTest extends ServicelayerBaseTest
{
	@Test
	public void shouldLoadUrlPathHelperViaClassLoader()
			throws ClassNotFoundException, InvocationTargetException, IllegalAccessException, NoSuchMethodException
	{
		final ClassLoader classLoader = this.getClass().getClassLoader();
		assertThat(classLoader).isInstanceOf(PlatformInPlaceClassLoader.class);

		final Class<?> aClass = classLoader.loadClass("org.springframework.web.util.UrlPathHelper");
		assertThat(aClass).isNotNull();

		final Method sapVersionToken = aClass.getMethod("SAPVersionToken");
		final Object invoke = sapVersionToken.invoke(null);
		assertThat(invoke).isEqualTo("This class has been provided by SAP SE");

	}

	@Test
	public void shouldLoadUrlPathHelperClass()
			throws ClassNotFoundException, InvocationTargetException, IllegalAccessException, NoSuchMethodException
	{
		final Class<?> aClass = Class.forName("org.springframework.web.util.UrlPathHelper");
		assertThat(aClass).isNotNull();

		final Method sapVersionToken = aClass.getMethod("SAPVersionToken");
		final Object invoke = sapVersionToken.invoke(null);
		assertThat(invoke).isEqualTo("This class has been provided by SAP SE");

	}
}
