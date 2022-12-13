/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util.tomcat;

import static org.apache.commons.lang3.RandomStringUtils.randomAlphanumeric;
import static org.apache.commons.lang3.StringUtils.defaultIfBlank;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.testframework.BulkPropertyConfigSwitcher;
import de.hybris.platform.util.tomcat.ConfigurableRfc6265CookieProcessorHandlerLoader.ConfigurableRfc6265CookieHandler;

import java.util.Optional;

import javax.servlet.http.Cookie;

import org.apache.commons.lang3.RandomStringUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

@IntegrationTest
public class ConfigurableRfc6265CookieProcessorHandlerLoaderTest extends ServicelayerBaseTest
{
	private final BulkPropertyConfigSwitcher properties = new BulkPropertyConfigSwitcher();

	@Before
	public void setUp() throws Exception
	{

	}

	@After
	public void tearDown() throws Exception
	{
		properties.switchAllBack();
	}

	@Test
	public void shouldReturnSameSiteValueIfEnabled()
	{
		enableSameSiteHandler();
		final ConfigurableRfc6265CookieHandler handler = new ConfigurableRfc6265CookieHandler(
				Registry.getCurrentTenant().getConfig());


		final Cookie cookie = Mockito.mock(Cookie.class);
		final Optional<String> sameSite = handler.getSameSiteParameter(cookie);

		assertThat(sameSite).isPresent();
	}

	@Test
	public void shouldNotReturnSameSiteValueIfDisabled()
	{
		disableSameSiteHandler();
		final ConfigurableRfc6265CookieHandler handler = new ConfigurableRfc6265CookieHandler(
				Registry.getCurrentTenant().getConfig());


		final Cookie cookie = Mockito.mock(Cookie.class);
		final Optional<String> sameSite = handler.getSameSiteParameter(cookie);

		assertThat(sameSite).isEmpty();
	}

	@Test
	public void shouldReturnDefaultValueIfNoOtherConfigProvided1()
	{
		enableSameSiteHandler();
		properties.switchToValue("cookies.SameSite", "None");

		final ConfigurableRfc6265CookieHandler handler = new ConfigurableRfc6265CookieHandler(
				Registry.getCurrentTenant().getConfig());

		final Cookie cookie = createRandomCookie();
		final Optional<String> sameSite = handler.getSameSiteParameter(cookie);

		assertThat(sameSite).contains("None");
	}

	@Test
	public void shouldReturnDefaultValueIfNoOtherConfigProvided2()
	{
		enableSameSiteHandler();
		properties.switchToValue("cookies.SameSite", "Strict");
		final ConfigurableRfc6265CookieHandler handler = new ConfigurableRfc6265CookieHandler(
				Registry.getCurrentTenant().getConfig());

		final Cookie cookie = createRandomCookie();
		final Optional<String> sameSite = handler.getSameSiteParameter(cookie);

		assertThat(sameSite).contains("Strict");
	}

	@Test
	public void shouldReturnValidValueIfManyProperties()
	{
		enableSameSiteHandler();

		final String domain = RandomStringUtils.randomAlphabetic(10);
		final String path = RandomStringUtils.randomAlphabetic(10);
		final String name = RandomStringUtils.randomAlphabetic(10);

		final Cookie c1 = createCookie(null, null, null);
		final Cookie c2 = createCookie(domain, null, null);
		final Cookie c3 = createCookie(domain, path, null);
		final Cookie c4 = createCookie(domain, path, name);

		properties.switchToValue("cookies.SameSite", "value1");
		properties.switchToValue("cookies." + domain + ".SameSite", "value2");
		properties.switchToValue("cookies." + domain + "." + path + ".SameSite", "value3");
		properties.switchToValue("cookies." + domain + "." + path + "." + name + ".SameSite", "value4");

		final ConfigurableRfc6265CookieHandler handler = new ConfigurableRfc6265CookieHandler(
				Registry.getCurrentTenant().getConfig());

		assertThat(handler.getSameSiteParameter(c1)).contains("value1");
		assertThat(handler.getSameSiteParameter(c2)).contains("value2");
		assertThat(handler.getSameSiteParameter(c3)).contains("value3");
		assertThat(handler.getSameSiteParameter(c4)).contains("value4");

	}

	private Cookie createRandomCookie()
	{
		return createCookie(null, null, null);
	}

	private Cookie createCookie(final String domain, final String path, final String name)
	{
		final Cookie cookie = Mockito.mock(Cookie.class);

		when(cookie.getDomain()).thenReturn(defaultIfBlank(domain, randomAlphanumeric(8)));
		when(cookie.getName()).thenReturn(defaultIfBlank(name, randomAlphanumeric(8)));
		when(cookie.getPath()).thenReturn(defaultIfBlank(path, randomAlphanumeric(8)));
		return cookie;
	}

	private void disableSameSiteHandler()
	{
		properties.switchToValue(ConfigurableRfc6265CookieProcessorHandlerLoader.PARAM_SAMESITE_ENABLED,
				Boolean.FALSE.toString());
	}

	private void enableSameSiteHandler()
	{
		properties.switchToValue(ConfigurableRfc6265CookieProcessorHandlerLoader.PARAM_SAMESITE_ENABLED,
				Boolean.TRUE.toString());
	}

}
