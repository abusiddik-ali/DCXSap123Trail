/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.masterserver.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;

import java.util.Collections;
import java.util.Optional;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

@UnitTest
public class RestStatisticsSenderTest
{

	public static final String SOME_DATA = "someData";
	public static final String PASSWORD = "nimda";
	private static final String ED = "ed";
	private static final String LID = "lid";
	@Mock
	RestTemplate restTemplate;

	@Before
	public void setUp()
	{
		MockitoAnnotations.initMocks(this);
	}

	@Test(expected = NullPointerException.class)
	public void shouldThrowExceptionWhenRestTemplateIsNotProvided()
	{
		final StatisticsPublisher.RestStatisticsSender statisticsSender = new StatisticsPublisher.RestStatisticsSender(
				null);
	}

	@Test
	public void shouldReturnTruePayloadIsEmpty()
	{
		final StatisticsPublisher.RestStatisticsSender statisticsSender = new StatisticsPublisher.RestStatisticsSender(
				restTemplate);

		final boolean sent = statisticsSender.sendStatistics(null);
		assertThat(sent).isTrue();
	}

	@Test
	public void shouldReturnTruePayloadIsValid()
	{
		when(restTemplate.postForEntity(anyString(), anyObject(), any())).thenReturn(
				new ResponseEntity("msg", HttpStatus.OK));
		final StatisticsPublisher.RestStatisticsSender statisticsSender = new StatisticsPublisher.RestStatisticsSender(
				restTemplate);

		final boolean sent = statisticsSender.sendStatistics(statisticsPayload());

		assertThat(sent).isTrue();
	}

	@Test
	public void shouldReturnFalsePayloadIsValidPostResponseIsNot2xx()
	{
		when(restTemplate.postForEntity(anyString(), anyObject(), any())).thenReturn(
				new ResponseEntity("msg", HttpStatus.NOT_FOUND));
		final StatisticsPublisher.RestStatisticsSender statisticsSender = new StatisticsPublisher.RestStatisticsSender(
				restTemplate);

		final boolean sent = statisticsSender.sendStatistics(statisticsPayload());

		assertThat(sent).isFalse();
	}


	@Test
	public void shouldReturnFalsePayloadIsValidPostThrownException()
	{
		when(restTemplate.postForEntity(anyString(), anyObject(), any())).thenThrow(Exception.class);
		final StatisticsPublisher.RestStatisticsSender statisticsSender = new StatisticsPublisher.RestStatisticsSender(
				restTemplate);

		final boolean sent = statisticsSender.sendStatistics(statisticsPayload());

		assertThat(sent).isFalse();
	}

	@Test
	public void shouldFieldsOfHttpRequestEntityBeProperlySet()
	{
		final StatisticsPublisher.RestStatisticsSender statisticsSender = new StatisticsPublisher.RestStatisticsSender(
				restTemplate);

		final Optional<HttpEntity<MultiValueMap<String, String>>> httpEntity = statisticsSender.prepareHttpRequest(
				statisticsPayload());

		assertThat(httpEntity.isPresent()).isTrue();
		final MultiValueMap<String, String> body = httpEntity.get().getBody();
		assertThat(body.get(ED)).isEqualTo(Collections.singletonList(SOME_DATA));
		assertThat(body.get(LID)).isEqualTo(Collections.singletonList(PASSWORD));
	}

	StatisticsPayload statisticsPayload()
	{
		return new StatisticsPayload(PASSWORD, "hybris.com", SOME_DATA);
	}

}
