/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.web;

import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.util.config.ConfigIntf;
import de.hybris.platform.util.config.FastHashMapConfig;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import org.junit.Before;
import org.junit.Test;

@UnitTest
public class CustomMediaHeaderConfiguratorTest extends ServicelayerBaseTest
{
	private final static String MIME_TYPE = "text/plain";
	private final static String FOLDER = "root";
	CustomMediaHeaderConfigurator customMediaHeaderConfigurator;
	HttpServletResponse httpServletResponse;

	@Before
	public void setUp()
	{
		httpServletResponse = mock(HttpServletResponse.class);
		customMediaHeaderConfigurator = spy(CustomMediaHeaderConfigurator.class);
	}

	@Test
	public void checkIfMimeConfiguredHeadersAreSetToResponse()
	{
		when(customMediaHeaderConfigurator.getTenantConfig()).thenReturn(dummyConfig());

		customMediaHeaderConfigurator.modifyResponseWithConfiguredHeaders(httpServletResponse, MIME_TYPE);

		verify(httpServletResponse, times(1)).setHeader(Headers.CUSTOM_MIME_HEADER_1.getHeaderName(),
				Headers.CUSTOM_MIME_HEADER_1.getHeaderValue());
		verify(httpServletResponse, times(1)).setHeader(Headers.CUSTOM_MIME_HEADER_2.getHeaderName(),
				Headers.CUSTOM_MIME_HEADER_2.getHeaderValue());
	}

	@Test
	public void checkIfMimeCouldDisablePreconfiguredHeaders()
	{
		final ConfigIntf dummyConfig = dummyConfig();
		dummyConfig.setParameter("media.set.mime.header." + MIME_TYPE + ".preheader1", "disable");
		dummyConfig.setParameter("media.set.mime.header." + MIME_TYPE + ".preheader2", "disable");
		dummyConfig.setParameter("media.set.mime.header." + MIME_TYPE + ".preheader3", "disable");

		when(customMediaHeaderConfigurator.getPreConfiguredHeaders()).thenReturn(dummyPreConfiguredHeaders());
		when(customMediaHeaderConfigurator.getTenantConfig()).thenReturn(dummyConfig);

		customMediaHeaderConfigurator.modifyResponseWithConfiguredHeaders(httpServletResponse, MIME_TYPE);

		dummyPreConfiguredHeaders().forEach((header, value) ->
				verify(httpServletResponse, never()).setHeader(header, value));
	}

	@Test
	public void checkIfFolderCouldDisablePreconfiguredHeaders()
	{
		final ConfigIntf dummyConfig = dummyConfig();
		dummyConfig.setParameter("media.set.folder.header." + FOLDER + ".preheader1", "disable");
		dummyConfig.setParameter("media.set.folder.header." + FOLDER + ".preheader2", "disable");
		dummyConfig.setParameter("media.set.folder.header." + FOLDER + ".preheader3", "disable");

		when(customMediaHeaderConfigurator.getPreConfiguredHeaders()).thenReturn(dummyPreConfiguredHeaders());
		when(customMediaHeaderConfigurator.getTenantConfig()).thenReturn(dummyConfig);

		customMediaHeaderConfigurator.modifyResponseWithConfiguredHeaders(httpServletResponse, MIME_TYPE, FOLDER);

		dummyPreConfiguredHeaders().forEach((header, value) ->
				verify(httpServletResponse, never()).setHeader(header, value));
	}

	@Test
	public void checkIfMimeTakesPrecedenceOverFolderForHeadersConfiguration()
	{
		final ConfigIntf dummyConfig = dummyConfig();
		dummyConfig.setParameter("media.set.folder.header." + FOLDER + ".preheader1", "disable");
		dummyConfig.setParameter("media.set.folder.header." + FOLDER + ".preheader2", "yolo");
		dummyConfig.setParameter("media.set.folder.header." + FOLDER + ".preheader3", "disable");

		dummyConfig.setParameter("media.set.mime.header." + MIME_TYPE + ".preheader1", "mimeRocks!");
		dummyConfig.setParameter("media.set.mime.header." + MIME_TYPE + ".preheader2", "mimeRocksAgain!");

		when(customMediaHeaderConfigurator.getPreConfiguredHeaders()).thenReturn(dummyPreConfiguredHeaders());
		when(customMediaHeaderConfigurator.getTenantConfig()).thenReturn(dummyConfig);

		customMediaHeaderConfigurator.modifyResponseWithConfiguredHeaders(httpServletResponse, MIME_TYPE, FOLDER);

		verify(httpServletResponse, times(1)).setHeader(eq("preheader1"), eq("mimeRocks!"));
		verify(httpServletResponse, times(1)).setHeader(eq("preheader2"), eq("mimeRocksAgain!"));
		verify(httpServletResponse, never()).setHeader(eq("preheader3"), anyString());
	}

	@Test
	public void checkIfMimeCouldBeNull()
	{
		when(customMediaHeaderConfigurator.getTenantConfig()).thenReturn(dummyConfig());

		customMediaHeaderConfigurator.modifyResponseWithConfiguredHeaders(httpServletResponse, null);

		verify(httpServletResponse, never()).setHeader(Headers.CUSTOM_MIME_HEADER_1.getHeaderName(),
				Headers.CUSTOM_MIME_HEADER_1.getHeaderValue());
		verify(httpServletResponse, never()).setHeader(Headers.CUSTOM_MIME_HEADER_2.getHeaderName(),
				Headers.CUSTOM_MIME_HEADER_2.getHeaderValue());
	}

	@Test
	public void checkIfMimeCouldBeEmpty()
	{
		when(customMediaHeaderConfigurator.getTenantConfig()).thenReturn(dummyConfig());

		customMediaHeaderConfigurator.modifyResponseWithConfiguredHeaders(httpServletResponse, StringUtils.EMPTY);

		verify(httpServletResponse, never()).setHeader(Headers.CUSTOM_MIME_HEADER_1.getHeaderName(),
				Headers.CUSTOM_MIME_HEADER_1.getHeaderValue());
		verify(httpServletResponse, never()).setHeader(Headers.CUSTOM_MIME_HEADER_2.getHeaderName(),
				Headers.CUSTOM_MIME_HEADER_2.getHeaderValue());
	}

	@Test
	public void checkIfMimeAndFolderCouldBeEmpty()
	{
		when(customMediaHeaderConfigurator.getTenantConfig()).thenReturn(dummyConfig());

		customMediaHeaderConfigurator.modifyResponseWithConfiguredHeaders(httpServletResponse, StringUtils.EMPTY,
				StringUtils.EMPTY);

		verify(httpServletResponse, never()).setHeader(Headers.CUSTOM_MIME_HEADER_1.getHeaderName(),
				Headers.CUSTOM_MIME_HEADER_1.getHeaderValue());
		verify(httpServletResponse, never()).setHeader(Headers.CUSTOM_MIME_HEADER_2.getHeaderName(),
				Headers.CUSTOM_MIME_HEADER_2.getHeaderValue());
	}

	@Test
	public void checkIfMimeAndFolderCouldBeNull()
	{
		when(customMediaHeaderConfigurator.getTenantConfig()).thenReturn(dummyConfig());

		customMediaHeaderConfigurator.modifyResponseWithConfiguredHeaders(httpServletResponse, null, null);

		verify(httpServletResponse, never()).setHeader(Headers.CUSTOM_MIME_HEADER_1.getHeaderName(),
				Headers.CUSTOM_MIME_HEADER_1.getHeaderValue());
		verify(httpServletResponse, never()).setHeader(Headers.CUSTOM_MIME_HEADER_2.getHeaderName(),
				Headers.CUSTOM_MIME_HEADER_2.getHeaderValue());
		verify(httpServletResponse, never()).setHeader(Headers.CUSTOM_MIME_HEADER_1.getHeaderName(),
				Headers.CUSTOM_MIME_HEADER_1.getHeaderValue());
		verify(httpServletResponse, never()).setHeader(Headers.CUSTOM_MIME_HEADER_2.getHeaderName(),
				Headers.CUSTOM_MIME_HEADER_2.getHeaderValue());
	}

	@Test
	public void checkIfFolderConfiguredHeadersAreSetToResponse()
	{
		when(customMediaHeaderConfigurator.getTenantConfig()).thenReturn(dummyConfig());

		customMediaHeaderConfigurator.modifyResponseWithConfiguredHeaders(httpServletResponse, StringUtils.EMPTY, FOLDER);

		verify(httpServletResponse, times(1)).setHeader(Headers.CUSTOM_FOLDER_HEADER_1.getHeaderName(),
				Headers.CUSTOM_FOLDER_HEADER_1.getHeaderValue());
		verify(httpServletResponse, times(1)).setHeader(Headers.CUSTOM_FOLDER_HEADER_2.getHeaderName(),
				Headers.CUSTOM_FOLDER_HEADER_2.getHeaderValue());
	}

	@Test
	public void testIfPreconfiguredHeadersAreSetToResponse()
	{
		when(customMediaHeaderConfigurator.getPreConfiguredHeaders()).thenReturn(dummyPreConfiguredHeaders());

		customMediaHeaderConfigurator.modifyResponseWithConfiguredHeaders(httpServletResponse);

		dummyPreConfiguredHeaders().forEach((header, value) ->
				verify(httpServletResponse, times(1)).setHeader(header, value));
	}

	@Test
	public void testIfPreconfiguredAndBlacklistedHeadersAreNotSetToResponse()
	{
		when(customMediaHeaderConfigurator.getPreConfiguredHeaders()).thenReturn(dummyPreConfiguredHeaders());
		when(customMediaHeaderConfigurator.getDisabledHeaders()).thenReturn(preConfigRelatedHeadersBlacklist());

		customMediaHeaderConfigurator.modifyResponseWithConfiguredHeaders(httpServletResponse);

		verify(httpServletResponse, never()).setHeader(anyString(), anyString());
	}

	@Test
	public void checkIfConfiguredHeadersAreSetToResponse()
	{
		when(customMediaHeaderConfigurator.getTenantConfig()).thenReturn(dummyConfig());

		customMediaHeaderConfigurator.modifyResponseWithConfiguredHeaders(httpServletResponse);

		Headers.getGlobalHeaders().forEach((header, value) ->
				verify(httpServletResponse, times(1)).setHeader(header, value));
	}

	@Test
	public void testIfConfiguredAndBlacklistedHeadersAreNotSetToResponse()
	{
		when(customMediaHeaderConfigurator.getPreConfiguredHeaders()).thenReturn(new HashMap<>());
		when(customMediaHeaderConfigurator.getTenantConfig()).thenReturn(dummyConfig());
		when(customMediaHeaderConfigurator.getDisabledHeaders()).thenReturn(dummyConfigRelatedHeadersBlacklist());

		customMediaHeaderConfigurator.modifyResponseWithConfiguredHeaders(httpServletResponse);

		verify(httpServletResponse, never()).setHeader(anyString(), anyString());
	}

	private ConfigIntf dummyConfig()
	{
		final Map<String, String> properties = new HashMap<>();
		properties.put("media.set.header." + Headers.CUSTOM_HEADER_1.getHeaderName(), Headers.CUSTOM_HEADER_1.getHeaderValue());
		properties.put("media.set.header." + Headers.CUSTOM_HEADER_2.getHeaderName(), Headers.CUSTOM_HEADER_2.getHeaderValue());
		properties.put("media.set.header." + Headers.CUSTOM_HEADER_3.getHeaderName(), Headers.CUSTOM_HEADER_3.getHeaderValue());
		properties.put("media.set.mime.header." + MIME_TYPE + "." + Headers.CUSTOM_MIME_HEADER_1.getHeaderName(),
				Headers.CUSTOM_MIME_HEADER_1.getHeaderValue());
		properties.put("media.set.mime.header." + MIME_TYPE + "." + Headers.CUSTOM_MIME_HEADER_2.getHeaderName(),
				Headers.CUSTOM_MIME_HEADER_2.getHeaderValue());
		properties.put("media.set.folder.header." + FOLDER + "." + Headers.CUSTOM_FOLDER_HEADER_1.getHeaderName(),
				Headers.CUSTOM_FOLDER_HEADER_1.getHeaderValue());
		properties.put("media.set.folder.header." + FOLDER + "." + Headers.CUSTOM_FOLDER_HEADER_2.getHeaderName(),
				Headers.CUSTOM_FOLDER_HEADER_2.getHeaderValue());
		return new FastHashMapConfig(properties);
	}

	private Set<String> dummyConfigRelatedHeadersBlacklist()
	{
		return Headers.getGlobalHeaders().keySet();
	}

	private Set<String> preConfigRelatedHeadersBlacklist()
	{
		return dummyPreConfiguredHeaders().keySet();
	}


	private Map<String, String> dummyPreConfiguredHeaders()
	{
		final Map<String, String> dummyHeaders = new HashMap<>();
		dummyHeaders.put("preheader1", "preHeaderValue1");
		dummyHeaders.put("preheader2", "preHeaderValue2");
		dummyHeaders.put("preheader3", "preHeaderValue3");
		return dummyHeaders;
	}

	enum Headers
	{
		CUSTOM_HEADER_1("customheader-1", "customvalue-1"),
		CUSTOM_HEADER_2("customheader-2", "customvalue-2"),
		CUSTOM_HEADER_3("customheader-3", "customvalue-3"),
		CUSTOM_MIME_HEADER_1("mime-customheader-1", "mimecustomvalue-1"),
		CUSTOM_MIME_HEADER_2("mime-customheader-2", "mimecustomvalue-2"),
		CUSTOM_FOLDER_HEADER_1("folder-customheader-1", "foldercustomheader-1"),
		CUSTOM_FOLDER_HEADER_2("folder-customheader-2", "foldercustomheader-2");

		String headerName;
		String headerValue;

		Headers(final String headerName, final String headerValue)
		{
			this.headerName = headerName;
			this.headerValue = headerValue;
		}

		static Map<String, String> getGlobalHeaders()
		{
			return Stream.of(Headers.values())
			             .filter(h -> !h.getHeaderName().startsWith("mime"))
			             .filter(h -> !h.getHeaderName().startsWith("folder"))
			             .collect(
					             Collectors.toMap(h -> h.headerName, h -> h.headerValue));
		}

		static Map<String, String> getAllHeaders()
		{
			return Stream.of(Headers.values())
			             .collect(
					             Collectors.toMap(h -> h.headerName, h -> h.headerValue));
		}

		String getHeaderName()
		{
			return headerName;
		}

		String getHeaderValue()
		{
			return headerValue;
		}
	}
}
