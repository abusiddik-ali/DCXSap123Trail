/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.hac.facade;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;

import java.io.IOException;
import java.util.Arrays;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.mock.web.MockHttpServletResponse;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class HacDryRunFacadeTest
{

	private final byte[] EMPTY_ZIP_MAGIC_NUMBER = { 'P', 'K', 0x05, 0x06 };

	@Test
	public void shouldCreateEmptyZipFileWithoutThrowingException() throws IOException
	{

		final HacDryRunFacade dryRunFacade = new HacDryRunFacade();
		final MockHttpServletResponse httpResponse = new MockHttpServletResponse();
		dryRunFacade.streamFileToResponse(httpResponse);

		final byte[] outputStream = Arrays.copyOfRange(httpResponse.getContentAsByteArray(), 0, EMPTY_ZIP_MAGIC_NUMBER.length);
		assertThat(outputStream).isEqualTo(EMPTY_ZIP_MAGIC_NUMBER);
	}


}
