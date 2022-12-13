/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.mediaweb;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.jalo.media.MediaManager;
import de.hybris.platform.media.exceptions.MediaNotFoundException;
import de.hybris.platform.media.services.MediaHeadersRegistry;
import de.hybris.platform.media.web.DefaultMediaFilterLogic;
import de.hybris.platform.media.web.MediaFilterLogicContext;
import de.hybris.platform.servicelayer.web.CustomMediaHeaderConfigurator;
import de.hybris.platform.servicelayer.web.WebAppMediaFilter;
import de.hybris.platform.util.config.ConfigIntf;
import de.hybris.platform.util.config.FastHashMapConfig;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.FilterChain;
import javax.servlet.ServletOutputStream;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;


@UnitTest
public class WebAppMediaFilterUnitTest
{
	private static final String LAST_MODIFIED_DATE = "Thu, 01 Jan 1970 00:00:01 GMT";
	private static final String FORCE_DOWNLOAD_DIALOG_FILE_EXTENSIONS = "xls";
	private static final String ALLOWED_EXTENSIONS_FOR_CLASSLOADER_KEY = "media.allowed.extensions.for.ClassLoader";
	private static final String ALLOWED_RESOURCE_PATH_FROM_CLASSLOADER = "/medias/fromjar/somepath/xxxx/file.png";
	private static final String NOT_ALLOWED_RESOURCE_PATH_FROM_CLASSLOADER = "/medias/fromjar/somepath/xxxx/file.foo";
	private static final String ALLOWED_EXTENSIONS_FOR_CLASSLOADER = "jpg,png,csv";
	protected static final String HEADER_X_CONTENT_OPTIONS = "X-Content-Type-Options";
	protected static final String NOSNIFF = "nosniff";

	private TestMediaFilter mediaFilter;
	@Mock
	private ConfigIntf config;
	@Mock
	private HttpServletRequest httpRequest;
	@Mock
	private HttpServletResponse httpResponse;
	@Mock
	private FilterChain chain;
	@Mock
	private ServletOutputStream servletOutputStream;
	@Mock
	private InputStream inputStream;
	@Mock
	private MediaManager.InputStreamWithSize inputStreamWithSize;
	@Mock
	private ServletOutputStream outputStream;
	@Mock
	private MediaManager mediaMgr;
	@Mock
	private DefaultMediaFilterLogic mediaFilterLogic;
	@Mock
	private MediaHeadersRegistry mediaHeadersRegistry;
	private Map<String, String> configuredHeaders;
	private boolean prettyURLLegacyMode;

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);
		this.mediaFilter = new TestMediaFilter();

		configuredHeaders = new HashMap<>();
		configuredHeaders.put("Last-Modified", LAST_MODIFIED_DATE);

		given(mediaHeadersRegistry.getHeaders()).willReturn(configuredHeaders);
		given(mediaMgr.getMediaHeadersRegistry()).willReturn(mediaHeadersRegistry);
		given(httpRequest.getParameter("context"))
				.willReturn("bWFzdGVyfHJvb3R8MTIzNDV8aW1hZ2UvanBlZ3xoMDEvaDAyL2Zvby5qcGd8cXdlcnR5MTIzNDU");
		//		given(config.getParameter("media.header.Last-Modified")).willReturn(LAST_MODIFIED_DATE);
		given(Integer.valueOf(inputStream.read(any(byte[].class)))).willReturn(Integer.valueOf(1), Integer.valueOf(0),
				Integer.valueOf(-1));

		prettyURLLegacyMode = false;
	}

	@Test
	public void shouldPrintNotAllowedMessageWhenRequestingResourceFromClassloaderAndItDoesntContainAllowedExtension()
			throws Exception
	{
		// given
		given(httpRequest.getServletPath()).willReturn(NOT_ALLOWED_RESOURCE_PATH_FROM_CLASSLOADER);
		given(httpResponse.getOutputStream()).willReturn(servletOutputStream);
		given(config.getParameter(ALLOWED_EXTENSIONS_FOR_CLASSLOADER_KEY)).willReturn(ALLOWED_EXTENSIONS_FOR_CLASSLOADER);

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verify(httpResponse, times(1)).setContentType("text/plain");
		verifyIfHeaderXContentTypeOptionsIsSetToNoSniffForHttpResponse(httpResponse);
		verifyIfConfiguredHeadersAreSetForResponse(httpResponse);
		verify(servletOutputStream, times(1))
				.println("not allowed to load media '/somepath/xxxx/file.foo' from classloader. Check parameter "
						+ ALLOWED_EXTENSIONS_FOR_CLASSLOADER_KEY
						+ " in advanced.properties to change the file extensions (e.g. *.gif) that are allowed to download.");
	}

	@Test
	public void shouldPrintFileNotFoundMessageWhenRequestingResourceFromClassloaderAndItDoesntExist() throws Exception
	{
		// given
		given(httpRequest.getServletPath()).willReturn(ALLOWED_RESOURCE_PATH_FROM_CLASSLOADER);
		given(httpResponse.getOutputStream()).willReturn(servletOutputStream);
		given(config.getParameter(ALLOWED_EXTENSIONS_FOR_CLASSLOADER_KEY)).willReturn(ALLOWED_EXTENSIONS_FOR_CLASSLOADER);

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verify(httpResponse, times(1)).setContentType("text/plain");
		verifyIfHeaderXContentTypeOptionsIsSetToNoSniffForHttpResponse(httpResponse);
		verifyIfConfiguredHeadersAreSetForResponse(httpResponse);
		verify(servletOutputStream, times(1)).println("file '/somepath/xxxx/file.png' not found!");
	}

	@Test
	public void shouldLoadFileFromClassloaderWhenRequestingResourceFromJarAndItExist() throws Exception
	{
		// given
		final byte[] buffer = new byte[IOUtils.DEFAULT_BUFFER_SIZE];
		given(httpRequest.getServletPath()).willReturn(ALLOWED_RESOURCE_PATH_FROM_CLASSLOADER);
		given(httpResponse.getOutputStream()).willReturn(servletOutputStream);
		given(config.getParameter(ALLOWED_EXTENSIONS_FOR_CLASSLOADER_KEY)).willReturn(ALLOWED_EXTENSIONS_FOR_CLASSLOADER);
		given(Integer.valueOf(inputStream.read(buffer))).willReturn(Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(-1));
		mediaFilter.setResourceStream(inputStream);

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verify(inputStream, times(3)).read(buffer);
		verify(servletOutputStream, times(1)).write(buffer, 0, 1);
		verify(servletOutputStream, times(1)).write(buffer, 0, 2);
		verify(inputStream, times(1)).close();
		verifyIfConfiguredHeadersAreSetForResponse(httpResponse);

	}

	@Test
	public void shouldSend404ResponseWhenRequestingNormalResourceAndItDoesntExist() throws Exception
	{
		// given
		given(httpRequest.getServletPath()).willReturn("/medias/realFilename.jpg");
		given(mediaMgr.getMediaAsStreamWithSize("root", "h01/h02/foo.jpg")).willThrow(new MediaNotFoundException("Media not found"));

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verify(httpResponse, times(1)).setStatus(HttpServletResponse.SC_NOT_FOUND);
		verifyIfHeaderXContentTypeOptionsIsSetToNoSniffForHttpResponse(httpResponse);
	}

	@Test
	public void shouldSend403WhenRequestTriesToAccessSecuredFolderViaNonSecureMediaFilter() throws Exception
	{
		// given
		// master|root|13355|text/plain|privatemedia/h67/hb0/8796158951454.txt|-
		given(httpRequest.getParameter("context"))
				.willReturn("bWFzdGVyfHJvb3R8MTMzNTV8dGV4dC9wbGFpbnxwcml2YXRlbWVkaWEvaDY3L2hiMC84Nzk2MTU4OTUxNDU0LnR4dHwt");

		given(httpRequest.getServletPath()).willReturn("/medias/realFilename.jpg");
		given(Boolean.valueOf(mediaMgr.isFolderConfiguredAsSecured("root"))).willReturn(Boolean.TRUE);

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verify(httpResponse, times(1)).setStatus(HttpServletResponse.SC_FORBIDDEN);
		verifyIfHeaderXContentTypeOptionsIsSetToNoSniffForHttpResponse(httpResponse);
	}


	@Test
	public void shouldSend404ResponseWhenRequestingNormalResourceAndUnderlyingStorageStrategyWillThrowMediaNotFoundException()
			throws Exception
	{
		// given
		given(httpRequest.getServletPath()).willReturn("/medias/realFilename.jpg");
		given(mediaMgr.getMediaAsStreamWithSize("root", "h01/h02/foo.jpg")).willThrow(new MediaNotFoundException("Media not found"));

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verify(httpResponse, times(1)).setStatus(HttpServletResponse.SC_NOT_FOUND);
		verifyIfHeaderXContentTypeOptionsIsSetToNoSniffForHttpResponse(httpResponse);
	}


	@Test
	public void shouldServeMediaFileAsStreamWhenRequestingNormalResource() throws Exception
	{
		// given
		given(httpRequest.getServletPath()).willReturn("/medias/realFilename.jpg");
		setUpMocksBehaviorForStandardMediaResource();

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verifyMocksForStandardMediaResource();
		verifyIfHeaderXContentTypeOptionsIsSetToNoSniffForHttpResponse(httpResponse);
		verifyIfConfiguredHeadersAreSetForResponse(httpResponse);
	}

	@Test
	public void shouldServeMediaFileAsStreamWhenRequestingNormalResourceWithPrettyURL() throws Exception
	{
		// given
		enablePrettyUrl();
		given(httpRequest.getServletPath())
				.willReturn("medias/sys_master/folder/folderPath/h01/h02/123456789/pretty-url-needs-extra-work.jpg");
		setUpMocksBehaviorForStandardMediaResource();
		given(mediaMgr.getMediaAsStreamWithSize("folder", "folderPath/h01/h02/123456789.jpg")).willReturn(inputStreamWithSize);

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verify(httpResponse, times(1)).setHeader("Last-Modified", LAST_MODIFIED_DATE);
		verify(httpResponse, times(1)).getOutputStream();
		verify(mediaMgr, times(1)).getMediaAsStreamWithSize("folder", "folderPath/h01/h02/123456789.jpg");
		verifyIfHeaderXContentTypeOptionsIsSetToNoSniffForHttpResponse(httpResponse);
		verifyIfConfiguredHeadersAreSetForResponse(httpResponse);
	}

	@Test
	public void shouldServeMediaFileAsStreamWhenRequestingNormalResourceWithPrettyURLFromFolderWithFancyName() throws Exception
	{
		// given
		enablePrettyUrl();
		given(httpRequest.getServletPath())
				.willReturn("medias/sys_master/f-olde2_3r/folderPath/h01/h02/123456789/pretty-url-needs-extra-work.jpg");
		setUpMocksBehaviorForStandardMediaResource();
		given(mediaMgr.getMediaAsStreamWithSize("f-olde2_3r", "folderPath/h01/h02/123456789.jpg")).willReturn(inputStreamWithSize);

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verify(httpResponse, times(1)).setHeader("Last-Modified", LAST_MODIFIED_DATE);
		verify(httpResponse, times(1)).getOutputStream();
		verify(mediaMgr, times(1)).getMediaAsStreamWithSize("f-olde2_3r", "folderPath/h01/h02/123456789.jpg");
		verifyIfHeaderXContentTypeOptionsIsSetToNoSniffForHttpResponse(httpResponse);
		verifyIfConfiguredHeadersAreSetForResponse(httpResponse);
	}

	@Test
	public void shouldServeMediaFileAsStreamWhenRequestingNormalResourceWithPrettyURLFromFancyFolderPath() throws Exception
	{
		// given
		enablePrettyUrl();
		given(httpRequest.getServletPath())
				.willReturn("medias/sys_master/folder/_f_o-l2d3erPath_/h01/h02/123456789/pretty-url-needs-extra-work.jpg");
		setUpMocksBehaviorForStandardMediaResource();
		given(mediaMgr.getMediaAsStreamWithSize("folder", "_f_o-l2d3erPath_/h01/h02/123456789.jpg")).willReturn(inputStreamWithSize);

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verify(httpResponse, times(1)).setHeader("Last-Modified", LAST_MODIFIED_DATE);
		verify(httpResponse, times(1)).getOutputStream();
		verify(mediaMgr, times(1)).getMediaAsStreamWithSize("folder", "_f_o-l2d3erPath_/h01/h02/123456789.jpg");
		verifyIfHeaderXContentTypeOptionsIsSetToNoSniffForHttpResponse(httpResponse);
		verifyIfConfiguredHeadersAreSetForResponse(httpResponse);
	}

	@Test
	public void shouldServeMediaFileAsStreamWhenRequestingNormalResourceWithPrettyURLFromRootMediaFolder() throws Exception
	{
		// given
		enablePrettyUrl();
		given(httpRequest.getServletPath()).willReturn(
				"medias/sys_master/root/h01/h02/123456789/pretty-url-needs-extra-work.jpg");
		setUpMocksBehaviorForStandardMediaResource();
		given(mediaMgr.getMediaAsStreamWithSize("root", "h01/h02/123456789.jpg")).willReturn(inputStreamWithSize);

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verify(httpResponse, times(1)).setHeader("Last-Modified", LAST_MODIFIED_DATE);
		verify(httpResponse, times(1)).getOutputStream();
		verify(mediaMgr, times(1)).getMediaAsStreamWithSize("root", "h01/h02/123456789.jpg");
		verifyIfHeaderXContentTypeOptionsIsSetToNoSniffForHttpResponse(httpResponse);
		verifyIfConfiguredHeadersAreSetForResponse(httpResponse);
	}

	@Test
	public void shouldSend400ResponseWhenRequestingResourceHasMissingTenantPartForPrettyURL() throws Exception
	{
		// given
		enablePrettyUrl();
		given(httpRequest.getServletPath()).willReturn("medias/folderPath/some.jpg");

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verify(httpResponse, times(1)).setStatus(HttpServletResponse.SC_BAD_REQUEST);
	}

	@Test
	public void shouldPassTheFilterChainIfRequestingResourceContaisNameMediasButItDoesNotMatchProperMediasPathPrefix()
			throws Exception
	{
		// given
		given(httpRequest.getServletPath()).willReturn("medias.zul");

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verify(chain, times(1)).doFilter(httpRequest, httpResponse);
	}

	@Test
	public void shouldSend400ResponseWhenRequestingResourceHasInvalidTenantIdPrefixForPrettyURL() throws Exception
	{
		// given
		enablePrettyUrl();
		given(httpRequest.getServletPath())
				.willReturn("medias/wrong_tenant_id/root/h01/h02/123456789/pretty-url-needs-extra-work.jpg");
		setUpMocksBehaviorForStandardMediaResource();

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then fine
		verify(httpResponse, times(1)).setStatus(HttpServletResponse.SC_BAD_REQUEST);
	}

	@Test
	public void shouldServeMediaFileAsStreamWithContentDispositionHeaderWhenRequestingXLSFile() throws Exception
	{
		// given
		given(httpRequest.getServletPath()).willReturn("/medias/sys_master/realFilename.xls");
		setUpMocksBehaviorForStandardMediaResource();

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verifyMocksForStandardMediaResource();
		verify(httpResponse, times(1)).addHeader("Content-Disposition", " attachment; filename=realFilename.xls");
		verifyIfHeaderXContentTypeOptionsIsSetToNoSniffForHttpResponse(httpResponse);
		verifyIfConfiguredHeadersAreSetForResponse(httpResponse);
	}

	@Test
	public void shouldSend304ResponseWhenRequestingResourceAndRequestContainMatchingETagHeader() throws Exception
	{
		// given
		given(httpRequest.getServletPath()).willReturn("/medias/realFilename.jpg");
		given(httpRequest.getHeader("If-None-Match")).willReturn("8a7101697a29d819b3e6c2cb0063d1b8");
		given(config.getParameter("media.header.Last-Modified")).willReturn(LAST_MODIFIED_DATE);

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verify(httpResponse, times(1)).setHeader("ETag", "8a7101697a29d819b3e6c2cb0063d1b8");
		verify(httpRequest, times(1)).getHeader("If-None-Match");
		verify(httpResponse, times(1)).setHeader("Last-Modified", LAST_MODIFIED_DATE);
		verify(httpResponse, times(1)).setStatus(HttpServletResponse.SC_NOT_MODIFIED);
		verifyIfConfiguredHeadersAreSetForResponse(httpResponse);
	}

	@Test
	public void shouldProcessStandardResponseWhenRequestingResourceAndResourcePathContainETagValueButRequestDoesNotHaveIfNonMatch()
			throws Exception
	{
		// given
		given(httpRequest.getServletPath()).willReturn("/medias/realFilename.jpg");
		given(httpRequest.getHeader("If-None-Match")).willReturn(null);
		setUpMocksBehaviorForStandardMediaResource();

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verifyMocksForStandardMediaResource();
		verifyIfHeaderXContentTypeOptionsIsSetToNoSniffForHttpResponse(httpResponse);
		verifyIfConfiguredHeadersAreSetForResponse(httpResponse);
	}

	@Test
	public void shouldSendBadRequestResponseStatusIfMalformedURLExceptionWillBeThrown() throws Exception
	{
		// given
		given(httpRequest.getServletPath()).willReturn("/medias/foobar.xls");
		setUpMocksBehaviorForStandardMediaResource();
		doThrow(new IllegalArgumentException()).when(httpResponse).addHeader("Content-Disposition",
				" attachment; filename=foobar.xls");

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verify(httpResponse, times(1)).setStatus(HttpServletResponse.SC_BAD_REQUEST);
	}

	@Test
	public void shouldAddConfiguredHeadersToResponse() throws Exception
	{
		// given
		configuredHeaders.put("cache-control", "max-age=3600");
		given(mediaMgr.getMediaHeadersRegistry()).willReturn(mediaHeadersRegistry);
		given(mediaHeadersRegistry.getHeaders()).willReturn(configuredHeaders);
		given(httpRequest.getServletPath()).willReturn("/medias/realFilename.jpg");
		setUpMocksBehaviorForStandardMediaResource();

		// when
		mediaFilter.doFilter(httpRequest, httpResponse, chain);

		// then
		verifyMocksForStandardMediaResource();
		verify(httpResponse, times(1)).setHeader("cache-control", "max-age=3600");
		verifyIfHeaderXContentTypeOptionsIsSetToNoSniffForHttpResponse(httpResponse);
		verifyIfConfiguredHeadersAreSetForResponse(httpResponse);
	}

	private void setUpMocksBehaviorForStandardMediaResource() throws IOException
	{
		given(config.getParameter("media.force.download.dialog.fileextensions")).willReturn(
				FORCE_DOWNLOAD_DIALOG_FILE_EXTENSIONS);
		given(config.getParameter("media.header.Last-Modified")).willReturn(LAST_MODIFIED_DATE);
		given(mediaMgr.getMediaAsStreamWithSize("root", "h01/h02/foo.jpg")).willReturn(inputStreamWithSize);
		given(httpResponse.getOutputStream()).willReturn(outputStream);
		given(inputStreamWithSize.getInputStream()).willReturn(inputStream);
		given(inputStreamWithSize.getSize()).willReturn(54321L);
	}

	private void verifyMocksForStandardMediaResource()
	{
		verify(httpResponse, times(1)).setContentLengthLong(54321L);
		verify(httpResponse, times(1)).setHeader("Last-Modified", LAST_MODIFIED_DATE);
		verify(httpResponse, times(0)).setStatus(HttpServletResponse.SC_FORBIDDEN);
		verify(httpResponse, times(0)).setStatus(HttpServletResponse.SC_BAD_REQUEST);
		verify(httpResponse, times(0)).setStatus(HttpServletResponse.SC_NOT_FOUND);
	}

	@Test
	public void shouldProcessNormalFilterChainIfRequestOrResponseAreNotHttpServletRequest() throws Exception
	{
		// given
		final ServletRequest request = null;
		final ServletResponse response = null;

		// when
		mediaFilter.doFilter(request, response, chain);

		// then

		verify(chain, times(1)).doFilter(request, response);
	}

	private void verifyIfConfiguredHeadersAreSetForResponse(final HttpServletResponse httpResponse)
	{
		final Map<String, String> preConfiguredHeaders = CustomMediaHeaderConfigurator.PreConfiguredHeaders.getPreConfiguredHeaders();
		assertThat(preConfiguredHeaders).isNotEmpty();
		preConfiguredHeaders.forEach((header, value)
				-> verify(httpResponse, times(1)).setHeader(header, value));
	}

	// X-Content-Type-Options should be always set together with content type
	private void verifyIfHeaderXContentTypeOptionsIsSetToNoSniffForHttpResponse(
			final HttpServletResponse httpResponse)
	{
		verify(httpResponse, times(1)).setContentType(anyString());
		verify(httpResponse, times(1)).setHeader(HEADER_X_CONTENT_OPTIONS, NOSNIFF);
	}

	private void enablePrettyUrl()
	{
		prettyURLLegacyMode = true;
		given(httpRequest.getParameter("context")).willReturn(null);
	}

	private class TestMediaFilter extends WebAppMediaFilter
	{
		private InputStream resourceStream;

		public void setResourceStream(final InputStream resourceStream)
		{
			this.resourceStream = resourceStream;
		}

		@Override
		protected void modifyResponseWithConfiguredHeaders(final HttpServletResponse httpResponse)
		{
			final CustomMediaHeaderConfigurator headerConfigurator = spy(CustomMediaHeaderConfigurator.class);
			when(headerConfigurator.getTenantConfig()).thenReturn(new FastHashMapConfig(Collections.emptyMap()));
			headerConfigurator.modifyResponseWithConfiguredHeaders(httpResponse);
		}

		@Override
		protected void modifyResponseWithConfiguredHeaders(final HttpServletResponse httpResponse, final String mime,
		                                                   final String folder)
		{
			final CustomMediaHeaderConfigurator headerConfigurator = spy(CustomMediaHeaderConfigurator.class);
			when(headerConfigurator.getTenantConfig()).thenReturn(new FastHashMapConfig(Collections.emptyMap()));
			headerConfigurator.modifyResponseWithConfiguredHeaders(httpResponse, mime, folder);
		}

		@Override
		protected void modifyResponseWithConfiguredHeaders(final HttpServletResponse httpResponse, final String mime)
		{
			final CustomMediaHeaderConfigurator headerConfigurator = spy(CustomMediaHeaderConfigurator.class);
			when(headerConfigurator.getTenantConfig()).thenReturn(new FastHashMapConfig(Collections.emptyMap()));
			headerConfigurator.modifyResponseWithConfiguredHeaders(httpResponse, mime);
		}

		@Override
		protected boolean isLegacyPrettyUrlSupport()
		{
			return prettyURLLegacyMode;
		}

		@Override
		protected void addContentType(final HttpServletResponse httpResponse,
		                              final MediaFilterLogicContext ctx)
		{
			httpResponse.setContentType(anyString());
			setXContentTypeOptionsHeader(httpResponse);
		}

		@Override
		protected ConfigIntf getConfig()
		{
			return config;
		}

		@Override
		protected InputStream getResourceAsStream(final String resourceName)
		{
			return resourceStream;
		}


		@Override
		protected void setSecureURLRendererForThread(final HttpServletRequest httpRequest)
		{
			// TODO this test isn't ready for it yet - need to add it later !!!
		}

		@Override
		protected MediaManager getMediaManager()
		{
			return mediaMgr;
		}

		@Override
		protected DefaultMediaFilterLogic getMediaFilterLogic()
		{
			return mediaFilterLogic;
		}
	}
}
