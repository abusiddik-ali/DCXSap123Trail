/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.masterserver.collector.system.impl;

import static de.hybris.platform.masterserver.collector.system.impl.SpringConfigOverviewCollector.CONTENT_CHARSET;
import static de.hybris.platform.masterserver.collector.system.impl.SpringConfigOverviewCollector.OVERVIEW_OBJECT_KEY;
import static de.hybris.platform.masterserver.collector.system.impl.SpringConfigOverviewCollector.OVERVIEW_STATS_ENCODING_KEY;
import static de.hybris.platform.masterserver.collector.system.impl.SpringConfigOverviewCollector.OVERVIEW_STATS_KEY;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyZeroInteractions;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.masterserver.collector.system.impl.SpringConfigOverviewCollector.Compressor;
import de.hybris.platform.testframework.TestUtils;

import java.nio.charset.Charset;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;

@UnitTest
public class SpringConfigOverviewCollectorUnitTest
{
	@Test
	public void shouldReturnInstantiableCharsetName()
	{
		final String givenOverview = givenOverview();
		final SpringConfigOverviewCollector givenCollector = givenEnabledCollectorWithoutCompression(givenOverview);

		final Map<String, Map<String, String>> springStats = givenCollector.collectStatistics();
		assertThat(springStats).isNotNull().isNotEmpty().containsKey(OVERVIEW_OBJECT_KEY);

		final Map<String, String> stats = springStats.get(OVERVIEW_OBJECT_KEY);
		assertThat(stats).isNotNull().isNotEmpty().containsKey(OVERVIEW_STATS_ENCODING_KEY);

		final String charsetName = stats.get(OVERVIEW_STATS_ENCODING_KEY);
		assertThat(charsetName).isNotNull().isNotEmpty();

		final Charset charset = Charset.forName(charsetName);

		assertThat(charset).isNotNull();
	}

	@Test
	public void shouldEncodeTheOverviewUsingBase64()
	{
		final String givenOverview = givenOverview();
		final SpringConfigOverviewCollector givenCollector = givenEnabledCollectorWithoutCompression(givenOverview);

		final String decodedOverview = getDecodedOverviewFrom(givenCollector);

		assertThat(decodedOverview).isNotNull().isNotEmpty().isEqualTo(givenOverview);
	}

	@Test
	public void shouldCompressTheOverviewUsingDeflateAlgorithm()
	{
		final String givenOverview = givenOverview();
		final SpringConfigOverviewCollector givenCollector = givenEnabledCollector(givenOverview);

		final String base64EncodedOverview = getOverviewFrom(givenCollector);
		assertThat(base64EncodedOverview).isNotNull().isNotEmpty();

		final byte[] compressedOverview = SpringOverviewTestHelper.base64Decode(base64EncodedOverview);
		assertThat(compressedOverview).isNotNull().isNotEmpty();

		final String overview = SpringOverviewTestHelper.decompress(compressedOverview, CONTENT_CHARSET);
		assertThat(overview).isNotNull().isNotEmpty().isEqualTo(givenOverview);
	}

	@Test
	public void shouldReturnEmptyOverviewStringWhenProvidedOverviewIsNull()
	{
		final SpringConfigOverviewCollector givenCollector = givenEnabledCollector((String)null);

		final String overview = getOverviewFrom(givenCollector);
		assertThat(overview).isNotNull().isEmpty();
	}

	@Test
	public void shouldReturnEmptyOverviewStringWhenProvidedOverviewIsEmpty()
	{
		final String givenOverview = givenOverview("");
		final SpringConfigOverviewCollector givenCollector = givenEnabledCollector(givenOverview);

		final String overview = getOverviewFrom(givenCollector);
		assertThat(overview).isNotNull().isEmpty();
	}

	@Test
	public void shouldReturnEmptyOverviewStringWhenProvidedOverviewContainsOnlyWhitespaces()
	{
		final String givenOverview = givenOverview(" \t\n");
		final SpringConfigOverviewCollector givenCollector = givenEnabledCollector(givenOverview);

		final String overview = getOverviewFrom(givenCollector);
		assertThat(overview).isNotNull().isEmpty();
	}

	@Test
	public void shouldReturnEmptyOverviewEvenInCaseOfRuntimeException()
	{
		final SpringConfigOverviewCollector givenCollector = givenEnabledCollector(() -> {
			throw new RuntimeException("Expected");
		});

		TestUtils.disableFileAnalyzer("RuntimeException expected in the logs", 200);

		final String overview = getOverviewFrom(givenCollector);
		assertThat(overview).isNotNull().isEmpty();
	}

	@Test
	public void shouldReturnEmptyMapAndNotInteractWithTheOverviewProviderWhenDisabled()
	{
		final Supplier<String> mockedProvider = mock(Supplier.class);
		final SpringConfigOverviewCollector givenCollector = givenDisabledCollector(mockedProvider);

		final Map<String, Map<String, String>> statistics = givenCollector.collectStatistics();
		assertThat(statistics).isNotNull().isEmpty();

		verifyZeroInteractions(mockedProvider);
	}

	@Test
	public void shouldCompressUnicodeCharacters()
	{
		final Compressor compressor = new Compressor();
		final String stringWithUnicodeCharacters = "żółć\u00A9A\u0FCAB";

		final byte[] compressedBytes = compressor.compress(stringWithUnicodeCharacters);

		final String decompressed = SpringOverviewTestHelper.decompress(compressedBytes, CONTENT_CHARSET);
		assertThat(decompressed).isNotNull().isNotEmpty().isEqualTo(stringWithUnicodeCharacters);
	}

	private SpringConfigOverviewCollector givenEnabledCollector(final String givenOverview)
	{
		return new SpringConfigOverviewCollector(() -> givenOverview, new Compressor()::compress, Boolean.TRUE::booleanValue);
	}

	private SpringConfigOverviewCollector givenEnabledCollectorWithoutCompression(final String givenOverview)
	{
		return new SpringConfigOverviewCollector(() -> givenOverview, String::getBytes, Boolean.TRUE::booleanValue);
	}

	private SpringConfigOverviewCollector givenEnabledCollector(final Supplier<String> overviewProvider)
	{
		return new SpringConfigOverviewCollector(overviewProvider, String::getBytes, Boolean.TRUE::booleanValue);
	}

	private SpringConfigOverviewCollector givenDisabledCollector(final Supplier<String> overviewProvider)
	{
		return new SpringConfigOverviewCollector(overviewProvider, String::getBytes, Boolean.FALSE::booleanValue);
	}

	private String getDecodedOverviewFrom(final SpringConfigOverviewCollector collector)
	{
		final String overview = getOverviewFrom(collector);

		final byte[] decodedOverviewBytes = SpringOverviewTestHelper.base64Decode(overview);
		assertThat(decodedOverviewBytes).isNotNull();

		return new String(decodedOverviewBytes);
	}

	private String getOverviewFrom(final SpringConfigOverviewCollector collector)
	{
		final Map<String, Map<String, String>> springStats = collector.collectStatistics();
		assertThat(springStats).isNotNull().isNotEmpty().containsKey(OVERVIEW_OBJECT_KEY);

		final Map<String, String> stats = springStats.get(OVERVIEW_OBJECT_KEY);
		assertThat(stats).isNotNull()
		                 .hasSize(2)
		                 .containsOnlyKeys(OVERVIEW_STATS_KEY, OVERVIEW_STATS_ENCODING_KEY);

		final String overview = stats.get(OVERVIEW_STATS_KEY);
		assertThat(overview).isNotNull();

		return overview;
	}

	private String givenOverview(final String... parts)
	{
		Objects.requireNonNull(parts);

		if (parts.length == 1)
		{
			return parts[0];
		}

		return UUID.randomUUID() + Stream.of(parts)
		                                 .filter(Objects::nonNull)
		                                 .collect(Collectors.joining(UUID.randomUUID().toString()));
	}
}