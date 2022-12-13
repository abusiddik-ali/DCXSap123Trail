/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.mediaweb;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.http.MediaType.IMAGE_JPEG_VALUE;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.media.url.impl.LocalMediaWebURLStrategy;
import de.hybris.platform.mediaweb.assertions.assertj.Assertions;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.web.WebAppMediaFilter;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.testframework.seed.TestDataCreator;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.List;

import javax.annotation.Resource;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.RandomUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.NameValuePair;
import org.apache.http.client.utils.URLEncodedUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.mock.web.MockFilterChain;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

@IntegrationTest
public class WebAppMediaFilterWebRootTest extends ServicelayerBaseTest
{
	private static final Logger LOG = LoggerFactory.getLogger(WebAppMediaFilterWebRootTest.class);
	private final PropertyConfigSwitcher prettyUrl = new PropertyConfigSwitcher("media.legacy.prettyURL");
	private final PropertyConfigSwitcher allowCustomMediaWebWebRoot = new PropertyConfigSwitcher("allow.custom.mediaweb.webroot");
	private final PropertyConfigSwitcher customWebRoot = new PropertyConfigSwitcher("mediaweb.webroot");
	private final String DEFAULT_WEBROOT = "/medias";
	@Resource
	LocalMediaWebURLStrategy localMediaWebURLStrategy;
	@Resource
	private MediaService mediaService;
	@Resource
	private ModelService modelService;
	private CatalogVersionModel catalogVersion;

	@Before
	public void setUp()
	{
		allowCustomMediaWebWebRoot.switchToValue("true");
	}

	@After
	public void tearDown() throws NoSuchFieldException, IllegalAccessException
	{
		allowCustomMediaWebWebRoot.switchBackToDefault();
		customWebRoot.switchBackToDefault();
		prettyUrl.switchBackToDefault();
		setMediaWebRootForURLStrategy(DEFAULT_WEBROOT);
	}

	@Test
	public void shouldReturnResponseForCustomEmptyWebRootWithNotAllowedCustomMediaWebRoot()
			throws IOException, ServletException, NoSuchFieldException, IllegalAccessException
	{
		final String webRoot = StringUtils.EMPTY;
		allowCustomMediaWebWebRoot.switchToValue("false");
		setPrettyUrlAndWebRoot(PrettyURL.ENABLED, webRoot);
		final MediaModel media = createMediaModel("someName.jpg");
		assertThat(media.getMime()).isEqualToIgnoringCase(IMAGE_JPEG_VALUE);

		final MockHttpServletRequest httpRequestForMedia = requestForMedia(media);
		final HttpServletResponse response = doFilter(httpRequestForMedia);
		Assertions.assertThat(httpRequestForMedia.getRequestURI()).startsWith(DEFAULT_WEBROOT);
		Assertions.assertThat(response).status().is2xxSuccessful();
		Assertions.assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}

	@Test
	public void shouldReturnNullContentForCustomWebRootWithNotAllowedCustomMediaWebRoot()
			throws IOException, ServletException, NoSuchFieldException, IllegalAccessException
	{
		final String webRoot = "/shop/media";
		allowCustomMediaWebWebRoot.switchToValue("false");
		setPrettyUrlAndWebRoot(PrettyURL.ENABLED, webRoot);
		final MediaModel media = createMediaModel("someName.jpg");
		assertThat(media.getMime()).isEqualToIgnoringCase(IMAGE_JPEG_VALUE);

		final MockHttpServletRequest httpRequestForMedia = requestForMedia(media);
		final HttpServletResponse response = doFilter(httpRequestForMedia);
		Assertions.assertThat(response).status().is2xxSuccessful();
		Assertions.assertThat(response).hasContentType(null);
	}


	@Test
	public void shouldReturnResponseForCustomSlashStartingWebRootWithPrettyURLEnabled()
			throws IOException, ServletException, NoSuchFieldException, IllegalAccessException
	{
		final String webRoot = "/shop/medias";
		setPrettyUrlAndWebRoot(PrettyURL.ENABLED, webRoot);
		final MediaModel media = createMediaModel("someName.jpg");
		assertThat(media.getMime()).isEqualToIgnoringCase(IMAGE_JPEG_VALUE);

		final MockHttpServletRequest httpRequestForMedia = requestForMedia(media);
		final HttpServletResponse response = doFilter(httpRequestForMedia);

		Assertions.assertThat(httpRequestForMedia.getRequestURI()).startsWith(webRoot);
		Assertions.assertThat(response).status().is2xxSuccessful();
		Assertions.assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}

	@Test
	public void shouldReturnResponseForCustomWebRootWithPrettyURLEnabled()
			throws IOException, ServletException, NoSuchFieldException, IllegalAccessException
	{
		final String webRoot = "shop/medias";
		setPrettyUrlAndWebRoot(PrettyURL.ENABLED, webRoot);

		final MediaModel media = createMediaModel("someName.jpg");
		assertThat(media.getMime()).isEqualToIgnoringCase(IMAGE_JPEG_VALUE);

		final MockHttpServletRequest httpRequestForMedia = requestForMedia(media);
		final HttpServletResponse response = doFilter(httpRequestForMedia);

		Assertions.assertThat(httpRequestForMedia.getRequestURI()).startsWith("/" + webRoot);
		Assertions.assertThat(response).status().is2xxSuccessful();
		Assertions.assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}

	@Test
	public void shouldReturnResponseForCustomWebRootWithPrettyURLDisabled()
			throws IOException, ServletException, NoSuchFieldException, IllegalAccessException
	{
		final String webRoot = "shop/medias";
		setPrettyUrlAndWebRoot(PrettyURL.DISABLED, webRoot);

		final MediaModel media = createMediaModel("someName.jpg");
		assertThat(media.getMime()).isEqualToIgnoringCase(IMAGE_JPEG_VALUE);

		final MockHttpServletRequest httpRequestForMedia = requestForMedia(media);
		final HttpServletResponse response = doFilter(httpRequestForMedia);

		Assertions.assertThat(httpRequestForMedia.getRequestURI()).startsWith("/" + webRoot);
		Assertions.assertThat(response).status().is2xxSuccessful();
		Assertions.assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}

	@Test
	public void shouldReturnResponseForCustomEmptyWebRootWithPrettyURLEnabled()
			throws IOException, ServletException, NoSuchFieldException, IllegalAccessException
	{
		final String webRoot = StringUtils.EMPTY;
		setPrettyUrlAndWebRoot(PrettyURL.ENABLED, webRoot);
		final MediaModel media = createMediaModel("someName.jpg");
		assertThat(media.getMime()).isEqualToIgnoringCase(IMAGE_JPEG_VALUE);

		final MockHttpServletRequest httpRequestForMedia = requestForMedia(media);
		final HttpServletResponse response = doFilter(httpRequestForMedia);
		Assertions.assertThat(httpRequestForMedia.getRequestURI()).startsWith(DEFAULT_WEBROOT);
		Assertions.assertThat(response).status().is2xxSuccessful();
		Assertions.assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}

	@Test
	public void shouldReturnResponseForCustomSlashStartingWebRootWithPrettyURLDisabled()
			throws IOException, ServletException, NoSuchFieldException, IllegalAccessException
	{
		final String webRoot = "/shop/medias";
		setPrettyUrlAndWebRoot(PrettyURL.DISABLED, webRoot);

		final MediaModel media = createMediaModel("someName.jpg");
		assertThat(media.getMime()).isEqualToIgnoringCase(IMAGE_JPEG_VALUE);

		final MockHttpServletRequest httpRequestForMedia = requestForMedia(media);
		final HttpServletResponse response = doFilter(httpRequestForMedia);

		Assertions.assertThat(httpRequestForMedia.getRequestURI()).startsWith(webRoot);
		Assertions.assertThat(response).status().is2xxSuccessful();
		Assertions.assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}


	@Test
	public void shouldReturnResponseForCustomFewSlashStartingWebRootWithPrettyURLDisabled()
			throws IOException, ServletException, NoSuchFieldException, IllegalAccessException
	{
		final String webRoot = "/shop/electron/medias";
		setPrettyUrlAndWebRoot(PrettyURL.DISABLED, webRoot);

		final MediaModel media = createMediaModel("someName.jpg");
		assertThat(media.getMime()).isEqualToIgnoringCase(IMAGE_JPEG_VALUE);

		final MockHttpServletRequest httpRequestForMedia = requestForMedia(media);
		final HttpServletResponse response = doFilter(httpRequestForMedia);

		Assertions.assertThat(httpRequestForMedia.getRequestURI()).startsWith(webRoot);
		Assertions.assertThat(response).status().is2xxSuccessful();
		Assertions.assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}

	@Test
	public void shouldReturnResponseForCustomEmptyWebRootWithPrettyURLDisabled()
			throws IOException, ServletException, NoSuchFieldException, IllegalAccessException
	{
		final String webRoot = StringUtils.EMPTY;
		setPrettyUrlAndWebRoot(PrettyURL.DISABLED, webRoot);

		final MediaModel media = createMediaModel("someName.jpg");
		assertThat(media.getMime()).isEqualToIgnoringCase(IMAGE_JPEG_VALUE);

		final MockHttpServletRequest httpRequestForMedia = requestForMedia(media);
		final HttpServletResponse response = doFilter(httpRequestForMedia);

		Assertions.assertThat(httpRequestForMedia.getRequestURI()).startsWith(DEFAULT_WEBROOT);
		Assertions.assertThat(response).status().is2xxSuccessful();
		Assertions.assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}

	private HttpServletResponse doFilter(final MockHttpServletRequest request) throws IOException, ServletException
	{
		LOG.info("calling request: {}", request);
		final MockHttpServletResponse response = new MockHttpServletResponse();
		new WebAppMediaFilter().doFilter(request, response, new MockFilterChain());
		return response;
	}

	private void setPrettyUrlAndWebRoot(final PrettyURL pUrl, final String webRoot)
			throws NoSuchFieldException, IllegalAccessException
	{
		// for Spring cfg
		setMediaWebRootForURLStrategy(webRoot);
		// for properties
		customWebRoot.switchToValue(webRoot);
		if (pUrl == PrettyURL.ENABLED)
		{
			prettyUrl.switchToValue("true");
		}
		else
		{
			prettyUrl.switchToValue("false");
		}
	}

	private void setMediaWebRootForURLStrategy(final String webRoot) throws NoSuchFieldException, IllegalAccessException
	{
		final Field mediaWebRoot = LocalMediaWebURLStrategy.class.getDeclaredField("mediaWebRoot");
		mediaWebRoot.setAccessible(true);
		mediaWebRoot.set(localMediaWebURLStrategy, webRoot);
		mediaWebRoot.setAccessible(false);
	}


	private MockHttpServletRequest requestForMedia(final MediaModel model)
	{
		final MockHttpServletRequest request = new MockHttpServletRequest();

		final String url = model.getURL();
		assertThat(url).isNotEmpty();

		final String[] parts = StringUtils.split(url, '?');
		if (ArrayUtils.getLength(parts) > 1)
		{
			final List<NameValuePair> parse = URLEncodedUtils.parse(parts[1], StandardCharsets.UTF_8);
			parse.forEach(n -> {
				LOG.info("{} = {}}", n.getName(), n.getValue());
				request.addParameter(n.getName(), n.getValue());
			});
		}
		request.setRequestURI(parts[0]);
		return request;
	}

	private MediaModel createMediaModel(final String originalName)
	{
		final TestDataCreator testDataCreator = new TestDataCreator(modelService);
		final CatalogModel catalog = testDataCreator.createCatalog();
		catalogVersion = testDataCreator.createCatalogVersion("testVersion", catalog);

		final MediaModel mediaModel = modelService.create(MediaModel.class);
		mediaModel.setCode("TEST_CODE");
		mediaModel.setLocation("randomLocation");
		mediaModel.setCatalogVersion(catalogVersion);
		modelService.saveAll();
		mediaService.setStreamForMedia(mediaModel, new ByteArrayInputStream(RandomUtils.nextBytes(50)), originalName, null);
		modelService.saveAll();
		modelService.detach(mediaModel);
		return mediaModel;
	}

	private enum PrettyURL
	{
		ENABLED, DISABLED;
	}
}
