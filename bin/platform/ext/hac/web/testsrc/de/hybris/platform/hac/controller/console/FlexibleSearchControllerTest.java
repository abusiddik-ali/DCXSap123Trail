/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.hac.controller.console;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.hac.data.dto.SqlSearchResultData;
import de.hybris.platform.hac.data.form.FlexSearchFormData;
import de.hybris.platform.hac.facade.HacFlexibleSearchFacade;
import de.hybris.platform.jdbcwrapper.HybrisDataSource;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.servicelayer.user.UserService;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.ui.Model;
import org.springframework.web.server.ResponseStatusException;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class FlexibleSearchControllerTest {

    @Mock
    private HacFlexibleSearchFacade flexibleSearchFacade;

    @Mock
    private I18NService i18nService;

    @Mock
    private UserService userService;

    @InjectMocks
    private FlexibleSearchController flexibleSearchController;

    @Before
    public void setUp() throws Exception {
        when(flexibleSearchFacade.getCompatibleSampleQueries()).thenReturn(List.of());
    }

    @Test
    public void testFlexSearchIsReturningReadOnlyDataSource()
    {
        // given
        final FlexSearchFormData form = new FlexSearchFormData();
        given(flexibleSearchFacade.getDefaultDataSource()).willReturn("readonly");


        // when
        flexibleSearchController.flexsearch(mock(Model.class), form, mock(HttpServletRequest.class));

        // then
        assertThat(form.getDataSource()).isEqualTo("readonly");
    }

    @Test
    public void testFlexSearchIsReturningMasterDataSource()
    {
        // given
        final FlexSearchFormData form = new FlexSearchFormData();
        given(flexibleSearchFacade.getDefaultDataSource()).willReturn("master");


        // when
        flexibleSearchController.flexsearch(mock(Model.class), form, mock(HttpServletRequest.class));

        // then
        assertThat(form.getDataSource()).isEqualTo("master");
    }

    @Test
    public void testExecuteFlexSearchWithEmptyDataSource()
    {
        // given
        final FlexSearchFormData data = new FlexSearchFormData();
        data.setDataSource("");
        final SqlSearchResultData sqlSearchResultData = new SqlSearchResultData();
        sqlSearchResultData.setDataSourceId("master");

        given(flexibleSearchFacade.executeFlexibleSearchQuery(anyString(), any(), any(), anyInt(), anyBoolean(), eq(""))).willReturn(sqlSearchResultData);

        // when
        final SqlSearchResultData actual = flexibleSearchController.executeFlexsearch(data);

        // then
        assertThat(actual.getDataSourceId()).isEqualTo("master");
    }

    @Test(expected = ResponseStatusException.class)
    public void textExecuteFlexSearchWithCommitTrueAndReadOnlyDataSource() {
        // given
        final FlexSearchFormData data = new FlexSearchFormData();
        data.setFlexibleSearchQuery("nonBlank");
        data.setCommit(true);
        data.setDataSource("F");

        given(flexibleSearchFacade.executeFlexibleSearchQuery(anyString(), any(), any(), anyInt(), eq(true), eq("F"))).willCallRealMethod();
        given(flexibleSearchFacade.executeFlexibleSearchQuery(anyString(), any(), any(), anyInt(), eq(true), any(HybrisDataSource.class))).willCallRealMethod();

        // when
        flexibleSearchController.executeFlexsearch(data);
    }
}