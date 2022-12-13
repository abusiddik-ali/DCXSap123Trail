/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.mediaweb;

import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.media.services.impl.DefaultMediaLocationHashService;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.web.WebAppMediaFilter;

import javax.annotation.Resource;
import javax.servlet.FilterChain;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

@IntegrationTest
public class MediaFilterErrorHandlingTest extends ServicelayerBaseTest
{
    @Mock
    private HttpServletRequest httpRequest;
    @Mock
    private HttpServletResponse httpResponse;
    @Mock
    private FilterChain chain;
	@Resource
	private DefaultMediaLocationHashService mediaLocationHashService;

	private final static String SALT = "35b5cd0da3121fc53b4bc84d0c8af2e81";
	private String saltBackup = null;
	private final MediaFilter mediaFilter = new TestMediaFilter();
    private final WebAppMediaFilter webAppMediaFilter = new WebAppMediaFilter();

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);
		backupSaltIfNeeded();
		mediaLocationHashService.setSalt(SALT);
	}

	private void backupSaltIfNeeded()
	{
		if (saltBackup == null)
		{
			saltBackup = mediaLocationHashService.getSalt();
		}
	}

	@After
	public void tearDown()
	{
		mediaLocationHashService.setSalt(saltBackup);
	}

	@Test
    public void shouldNotShowHttp500ErrorWhenAccessingNonExistingFileForMediaFilter() throws Exception
    {
        // given
        given(httpRequest.getServletPath()).willReturn("medias?");
        given(httpRequest.getParameter("context")).willReturn(
                "bWFzdGVyfHJvb3R8MTIwNDB8aW1hZ2UvanBlZ3xoODYvaGQ3LzI5MTI3MTcwMjYyNDB8YjY0NTU3MWVjMGVhY2U2ZmIxMTQxMGY3MGI3ZTYxYjBmOTY1ZmZjZDJmNDc0YTExNDY2ZjJlNWM0MmFmZDU2ZQ");

        // when
        mediaFilter.doFilter(httpRequest, httpResponse, null);

        // then fine
        verify(httpResponse, times(1)).setStatus(HttpServletResponse.SC_NOT_FOUND);
    }

    @Test
    public void shouldNotShowHttp500ErrorWhenAccessingNonExistingFileForWebAppMediaFilter() throws Exception
    {
        // given
        given(httpRequest.getServletPath()).willReturn("medias?");

        given(httpRequest.getParameter("context")).willReturn(
                "bWFzdGVyfHJvb3R8MTIwNDB8aW1hZ2UvanBlZ3xoODYvaGQ3LzI5MTI3MTcwMjYyNDB8YjY0NTU3MWVjMGVhY2U2ZmIxMTQxMGY3MGI3ZTYxYjBmOTY1ZmZjZDJmNDc0YTExNDY2ZjJlNWM0MmFmZDU2ZQ");

        // when
        webAppMediaFilter.doFilter(httpRequest, httpResponse, chain);

        // then fine
        verify(httpResponse, times(1)).setStatus(HttpServletResponse.SC_NOT_FOUND);
    }


    public static class TestMediaFilter extends MediaFilter {
	    @Override
	    protected void setCurretTenantByID(final Iterable<String> mediaContext)
	    {
		    //do not change tenant
	    }

	    @Override
	    protected void unsetCurrentTenant()
	    {
		    //do not change tenant
	    }
    }
}
