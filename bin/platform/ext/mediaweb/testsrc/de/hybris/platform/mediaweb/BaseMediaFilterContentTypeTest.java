/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.mediaweb;


import static de.hybris.platform.mediaweb.assertions.assertj.Assertions.assertThat;

import static org.springframework.http.MediaType.IMAGE_JPEG_VALUE;
import static org.springframework.http.MediaType.TEXT_HTML_VALUE;
import static org.springframework.http.MediaType.TEXT_PLAIN_VALUE;

import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.media.services.MediaLocationHashService;
import de.hybris.platform.media.url.impl.LocalMediaWebURLStrategy;
import de.hybris.platform.media.web.MediaFilterLogicContext;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.testframework.seed.TestDataCreator;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Objects;

import javax.annotation.Resource;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.RandomUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.NameValuePair;
import org.apache.http.client.utils.URLEncodedUtils;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.mock.web.MockHttpServletRequest;

import com.google.common.base.Splitter;


public abstract class BaseMediaFilterContentTypeTest extends ServicelayerBaseTest
{
	private static final Logger LOG = LoggerFactory.getLogger(BaseMediaFilterContentTypeTest.class);
	private CatalogVersionModel catalogVersion;

	@Resource
	private MediaService mediaService;

	@Resource
	private ModelService modelService;

	@Resource
	private MediaLocationHashService mediaLocationHashService;

	@Before
	public void setUp() throws Exception
	{
		final TestDataCreator testDataCreator = new TestDataCreator(modelService);
		final CatalogModel catalog = testDataCreator.createCatalog();
		catalogVersion = testDataCreator.createCatalogVersion("testVersion", catalog);

	}

	@Test
	public void shouldHaveJpegContentTypeForJpegFileNameAndSameMimeType() throws IOException, ServletException
	{
		final MediaModel media = createMediaModel("someName.jpg");
		assertThat(media.getMime()).isEqualToIgnoringCase(IMAGE_JPEG_VALUE);

		final HttpServletResponse response = doFilter(requestForMedia(media));

		assertThat(response).status().is2xxSuccessful();
		assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}

	@Test
	public void shouldReturnMimeTypeFromDBEvenIfNotMatchingTheFileName() throws IOException, ServletException
	{
		final MediaModel media = createMediaModel("someName.jpg");
		assertMediaHasMime(media.getPk(), IMAGE_JPEG_VALUE);
		setMimeType(media.getPk(), TEXT_PLAIN_VALUE);
		assertMediaHasMime(media.getPk(), TEXT_PLAIN_VALUE);

		final HttpServletResponse response = doFilter(requestForMedia(media));

		assertThat(response).status().is2xxSuccessful();
		assertThat(response).hasContentType(TEXT_PLAIN_VALUE);
	}

	@Test
	public void shouldReturnDetectedMimeTypeWhenDBFieldIsEmpty() throws IOException, ServletException
	{
		final MediaModel media = createMediaModel("someName.jpg");
		assertMediaHasMime(media.getPk(), IMAGE_JPEG_VALUE);

		setMimeType(media.getPk(), null);
		assertMediaHasMime(media.getPk(), null);

		final MockHttpServletRequest request = requestForMedia(media);

		final HttpServletResponse response = doFilter(request);

		assertThat(response).status().is2xxSuccessful();
		assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}

	@Test
	public void shouldReturnDetectedMimeTypeBaseOnContentWhenDBFieldIsEmpty() throws IOException, ServletException
	{
		final MediaModel media = createMediaModel("someName", getClass().getResourceAsStream("/test/files/photo.jpg"));
		assertMediaHasMime(media.getPk(), IMAGE_JPEG_VALUE);

		setMimeType(media.getPk(), null);
		assertMediaHasMime(media.getPk(), null);

		final MockHttpServletRequest request = requestForMedia(media);

		final HttpServletResponse response = doFilter(request);

		assertThat(response).status().is2xxSuccessful();
		assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}

	@Test
	public void shouldReturnDetectedMimeTypeWhenDBFieldIsEmptyAndHashedLocation() throws IOException, ServletException
	{
		final MediaModel media = createMediaModel("someName.jpg");
		assertMediaHasMime(media.getPk(), IMAGE_JPEG_VALUE);

		setMimeType(media.getPk(), null);
		assertMediaHasMime(media.getPk(), null);

		final MockHttpServletRequest request = requestForMedia(media);
		tamperContentTypeInMediaRequest(request, TEXT_PLAIN_VALUE);
		tamperHashInMediaRequest(request, calculateHashForFolderAndLocation(media.getPk()));

		final HttpServletResponse response = doFilter(request);

		assertThat(response).status().is2xxSuccessful();
		assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}

	@Test
	public void shouldReturnMimeTypeFromDBIfContextIsTamperedAndHashedLocation() throws IOException, ServletException
	{
		final MediaModel media = createMediaModel("someName.jpg");
		assertThat(media.getMime()).isEqualToIgnoringCase(IMAGE_JPEG_VALUE);

		final MockHttpServletRequest request = requestForMedia(media);
		tamperContentTypeInMediaRequest(request, TEXT_HTML_VALUE);
		tamperHashInMediaRequest(request, calculateHashForFolderAndLocation(media.getPk()));

		final HttpServletResponse response = doFilter(request);

		assertThat(response).status().is2xxSuccessful();
		assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}

	@Test
	public void shouldReturnDetectedMimeTypeBasedOnDataIfNoMimeOnDBAndNoExtensionAndHashedLocation() throws IOException, ServletException
	{
		final MediaModel media = createMediaModel("someName", getClass().getResourceAsStream("/test/files/photo.jpg"));
		setMimeType(media.getPk(), null);
		assertMediaHasMime(media.getPk(), null);

		final MockHttpServletRequest request = requestForMedia(media);
		//tamperContentTypeInMediaRequest(request, null);
		tamperHashInMediaRequest(request, calculateHashForFolderAndLocation(media.getPk()));

		final HttpServletResponse response = doFilter(request);

		assertThat(response).status().is2xxSuccessful();
		assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}

	@Test
	public void shouldReturnGivenMimeTypeIfValidHash() throws IOException, ServletException
	{
		final MediaModel media = createMediaModel("someName.jpg");
		assertMediaHasMime(media.getPk(), IMAGE_JPEG_VALUE);
		//do not recalculate hash
		setMimeType(media.getPk(), TEXT_HTML_VALUE, false);
		assertMediaHasMime(media.getPk(), TEXT_HTML_VALUE);

		final MockHttpServletRequest request = requestForMedia(media);
		//give valid mime in context
		tamperContentTypeInMediaRequest(request, IMAGE_JPEG_VALUE);

		final HttpServletResponse response = doFilter(request);

		assertThat(response).status().is2xxSuccessful();
		assertThat(response).hasContentType(IMAGE_JPEG_VALUE);
	}

	@Test
	public void shouldReturn403WhenMimeTypeInDBIsEmptyAndContextIsTampered() throws IOException, ServletException
	{
		final MediaModel media = createMediaModel("someName.jpg");
		assertMediaHasMime(media.getPk(), IMAGE_JPEG_VALUE);
		setMimeType(media.getPk(), null);
		assertMediaHasMime(media.getPk(), null);

		final MockHttpServletRequest request = requestForMedia(media);
		tamperContentTypeInMediaRequest(request, IMAGE_JPEG_VALUE);

		final HttpServletResponse response = doFilter(request);

		assertThat(response).status().isEqualTo(HttpStatus.FORBIDDEN);
	}

	@Test
	public void shouldReturn403WhenMimeTypeIsTampered() throws IOException, ServletException
	{
		final MediaModel media = createMediaModel("someName.jpg");
		assertThat(media.getMime()).isEqualToIgnoringCase(IMAGE_JPEG_VALUE);

		final MockHttpServletRequest request = requestForMedia(media);
		tamperContentTypeInMediaRequest(request, TEXT_HTML_VALUE);

		final HttpServletResponse response = doFilter(request);

		assertThat(response).status().isEqualTo(HttpStatus.FORBIDDEN);
	}


	private void tamperHashInMediaRequest(final MockHttpServletRequest request, final String hash)
	{
		tamperContextInMediaRequest(request, MediaFilterLogicContext.ContextPart.LOCATION_HASH, hash);
	}

	protected abstract HttpServletResponse doFilter(final MockHttpServletRequest request) throws IOException, ServletException;

	private MediaModel createMediaModel(final String originalName)
	{
		return createMediaModel(originalName, new ByteArrayInputStream(RandomUtils.nextBytes(50)), null);
	}

	private MediaModel createMediaModel(final String originalName, final InputStream data)
	{
		return createMediaModel(originalName, data, null);
	}

	private MediaModel createMediaModel(final String originalName, final String contentType)
	{
		return createMediaModel(originalName, new ByteArrayInputStream(RandomUtils.nextBytes(50)), contentType);
	}

	private MediaModel createMediaModel(final String originalName, final InputStream data, final String contentType)
	{
		final MediaModel mediaModel = modelService.create(MediaModel.class);
		if (StringUtils.isNotBlank(contentType))
		{
			mediaModel.setMime(contentType);
		}
		mediaModel.setCode("TEST_CODE");
		mediaModel.setLocation("randomLocation");
		mediaModel.setCatalogVersion(catalogVersion);
		modelService.saveAll();
		mediaService.setStreamForMedia(mediaModel, data, originalName, contentType);
		modelService.saveAll();
		modelService.detach(mediaModel);
		return mediaModel;
	}

	private void tamperContentTypeInMediaRequest(final MockHttpServletRequest request,
	                                             final String contextContentType)
	{
		tamperContextInMediaRequest(request, MediaFilterLogicContext.ContextPart.MIME, contextContentType);
	}


	private String calculateHashForFolderAndLocation(final String folderQualifier, final String location)
	{
		return mediaLocationHashService.createHashForLocation(folderQualifier, location);
	}

	private String calculateHashForFolderAndLocation(final PK mediaPk)
	{
		final MediaModel media = modelService.get(mediaPk);
		return calculateHashForFolderAndLocation(media.getFolder().getQualifier(), media.getLocation());
	}

	private void tamperContextInMediaRequest(
			final MockHttpServletRequest request, final MediaFilterLogicContext.ContextPart contextPart, final String contextContentType)
	{
		final String encodedContext = request.getParameter("context");
		assertThat(encodedContext).isNotNull().isNotEmpty();

		final List<String> context = new ArrayList<>();
		decodeContext(encodedContext).forEach(context::add);
		assertThat(context).hasSize(6);

		context.set(contextPart.getPartNumber(), contextContentType);

		request.setParameter("context", encodeContext(context));
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

	private Iterable<String> decodeContext(final String encodedParam)
	{
		Objects.requireNonNull(encodedParam);

		return Splitter.on(LocalMediaWebURLStrategy.CONTEXT_PARAM_DELIM)
		               .split(new String(Base64.getDecoder().decode(encodedParam), StandardCharsets.UTF_8));

	}

	private String encodeContext(final Iterable<String> context)
	{
		return Base64.getEncoder()
		             .encodeToString(StringUtils.join(context, LocalMediaWebURLStrategy.CONTEXT_PARAM_DELIM)
		                                        .getBytes(StandardCharsets.UTF_8));
	}

	private void assertMediaHasMime(final PK pk, final String textPlainValue)
	{
		assertThat(((MediaModel) modelService.get(pk)).getMime()).isEqualToIgnoringCase(textPlainValue);
	}

	private void setMimeType(final PK pk, final String mime)
	{
		setMimeType(pk, mime, true);
	}

	private void setMimeType(final PK pk, final String mime, final boolean recalculateHash)
	{
		final MediaModel o = modelService.get(pk);
		o.setMime(mime);
		if(recalculateHash)
		{
			//recalculate hash, because it is now dependant on mime
			o.setLocationHash(
					mediaLocationHashService.createHash(o.getFolder().getQualifier(), o.getLocation(), o.getSize(), mime));
		}
		modelService.save(o);
	}
}
