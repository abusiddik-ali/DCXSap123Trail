/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.genericsearch.impl;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.GenericQuery;
import de.hybris.platform.core.GenericSelectField;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.genericsearch.GenericSearchQuery;
import de.hybris.platform.genericsearch.GenericSearchService;
import de.hybris.platform.jalo.flexiblesearch.internal.ReadOnlyConditionsHelper;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import javax.annotation.Resource;
import java.util.ArrayList;

import static org.assertj.core.api.Assertions.assertThat;

@IntegrationTest
public class DefaultGenericSearchServiceWithoutTransactionTest extends ServicelayerBaseTest {


	private static final String READ_ONLY_DATASOURCE_ID = "f";
    private final PropertyConfigSwitcher genericSearchReadReplicaEnabled = new PropertyConfigSwitcher(DefaultGenericSearchService.GENERIC_SEARCH_READ_REPLICA_ENABLED);
    private final PropertyConfigSwitcher genericSearchReadReplicaTypeCodesExcluded = new PropertyConfigSwitcher(DefaultGenericSearchService.GENERIC_SEARCH_READ_REPLICA_TYPE_CODES_EXCLUDE);
	private final PropertyConfigSwitcher flexibleSearchReadOnlyDataSource = new PropertyConfigSwitcher(
			ReadOnlyConditionsHelper.PARAM_FS_READ_ONLY_DATASOURCE);

    @Resource
    private CommonI18NService commonI18NService;
    @Resource
    private I18NService i18nService;
    @Resource
    private GenericSearchService genericSearchService;
    @Resource
    private ConfigurationService configurationService;

	@Before
	public void setUp() throws Exception
	{
		flexibleSearchReadOnlyDataSource.switchToValue(READ_ONLY_DATASOURCE_ID);
	}

    @Test
    public void shouldBeExecutedAgainstReadOnlyWhenOtherTypesAreExcluded()
    {
        //given
        final GenericSearchQuery gsquery = createGSQuery();

        genericSearchReadReplicaEnabled.switchToValue("true");
        genericSearchReadReplicaTypeCodesExcluded.switchToValue("Employee,Customer");

        //when
        final SearchResult<ArrayList<String>> actual = genericSearchService.search(gsquery);

        //then
        assertThat(actual.getDataSourceId()).isEqualTo(READ_ONLY_DATASOURCE_ID);
    }

    @Test
    public void shouldBeExecutedAgainstReadOnlyWhenNoTypeIsExcluded()
    {
        //given
        final GenericSearchQuery gsquery = createGSQuery();

        genericSearchReadReplicaEnabled.switchToValue("true");

        //when
        final SearchResult<ArrayList<String>> actual = genericSearchService.search(gsquery);

        //then
        assertThat(actual.getDataSourceId()).isEqualTo(READ_ONLY_DATASOURCE_ID);
    }

    @Test
    public void shouldBeExecutedAgainstMasterWhenReadOnlyIsDisabledForBackoffice()
    {
        //given
        final GenericSearchQuery gsquery = createGSQuery();

        genericSearchReadReplicaEnabled.switchToValue("false");

        //when
        final SearchResult<ArrayList<String>> actual = genericSearchService.search(gsquery);

        //then
        assertThat(actual.getDataSourceId()).isEqualTo("master");
    }

    @Test
    public void shouldBeExecutedAgainstMasterWhenQueriedItemTypeIsExcluded()
    {
        //given
        final GenericSearchQuery gsquery = createGSQuery();

        genericSearchReadReplicaEnabled.switchToValue("true");
        genericSearchReadReplicaTypeCodesExcluded.switchToValue("Product");

        //when
        final SearchResult<ArrayList<String>> actual = genericSearchService.search(gsquery);

        //then
        assertThat(actual.getDataSourceId()).isEqualTo("master");
    }

    @Test
    public void shouldBeExecutedAgainstReadOnlyWhenPropertyBackofficeSearchReadReplicaIsMissing()
    {
        //given
        final GenericSearchQuery gsquery = createGSQuery();
        configurationService.getConfiguration().clearProperty(DefaultGenericSearchService.GENERIC_SEARCH_READ_REPLICA_ENABLED);

        //when
        final SearchResult<ArrayList<String>> actual = genericSearchService.search(gsquery);

        //then
        assertThat(actual.getDataSourceId()).isEqualTo(READ_ONLY_DATASOURCE_ID);
    }

    @Test
    public void shouldBeExecutedAgainstReadOnlyWhenPropertyBackofficeSearchReadReplicaIsNotBoolean()
    {
        //given
        final GenericSearchQuery gsquery = createGSQuery();
        genericSearchReadReplicaEnabled.switchToValue("nonBoolean");

        //when
        final SearchResult<ArrayList<String>> actual = genericSearchService.search(gsquery);

        //then
        assertThat(actual.getDataSourceId()).isEqualTo(READ_ONLY_DATASOURCE_ID);
    }

    private GenericSearchQuery createGSQuery()
    {
        final GenericSelectField codeSelectField = new GenericSelectField(ProductModel._TYPECODE, ProductModel.CODE,
                String.class);
        final GenericSelectField nameSelectField = new GenericSelectField(ProductModel._TYPECODE, ProductModel.NAME,
                String.class);

        final GenericQuery query = new GenericQuery(ProductModel._TYPECODE);
        query.addSelectField(codeSelectField);
        query.addSelectField(nameSelectField);

        final GenericSearchQuery gsquery = new GenericSearchQuery(query);
        return gsquery;
    }

    @After
    public void tearDown() throws Exception
    {
        genericSearchReadReplicaEnabled.switchBackToDefault();
        genericSearchReadReplicaTypeCodesExcluded.switchBackToDefault();
        flexibleSearchReadOnlyDataSource.switchBackToDefault();
    }
}
