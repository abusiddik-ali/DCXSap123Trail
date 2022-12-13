package de.hybris.platform.hac.facade;

import com.google.common.collect.Sets;
import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.core.model.user.EmployeeModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.hac.data.dto.SqlSearchResultData;
import de.hybris.platform.jalo.flexiblesearch.internal.ReadOnlyConditionsHelper;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.Config;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.UUID;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

@IntegrationTest
public class HacFlexibleSearchFacadeTest extends ServicelayerBaseTest {

    private final String EXAMPLE_FS_QUERY = "SELECT {PK} FROM {Product}";

	private final PropertyConfigSwitcher flexibleSearchReadOnlyDataSource = new PropertyConfigSwitcher(
			ReadOnlyConditionsHelper.PARAM_FS_READ_ONLY_DATASOURCE);

    @Resource
    private FlexibleSearchService flexibleSearchService;

    @Resource
    private CatalogVersionService catalogVersionService;

    @Resource
    private SessionService sessionService;

    @Resource
    private I18NService i18nService;

    @Resource(name = "defaultUserService")
    private UserService userService;

    @Resource
    private ModelService modelService;

    @Resource
    private HacDatabaseFacade hacDatabaseFacade;

    private HacFlexibleSearchFacade hacFlexibleSearchFacade;

    private UserModel employee;

    @Before
    public void setUp()  {
    	flexibleSearchReadOnlyDataSource.switchToValue("F");
        hacFlexibleSearchFacade = new HacFlexibleSearchFacade();
        hacFlexibleSearchFacade.setFlexibleSearchService(flexibleSearchService);
        hacFlexibleSearchFacade.setCatalogVersionService(catalogVersionService);
        hacFlexibleSearchFacade.setSessionService(sessionService);
        hacFlexibleSearchFacade.setUserService(userService);
        hacFlexibleSearchFacade.setI18nService(i18nService);
        hacFlexibleSearchFacade.setDatabaseFacade(hacDatabaseFacade);

        final PrincipalGroupModel adminGroup = userService.getAdminUserGroup();

        employee = modelService.create(EmployeeModel.class);
        final String value = UUID.randomUUID().toString();
        employee.setUid(value);
        employee.setName(value);
        employee.setGroups(Sets.newHashSet(adminGroup));
        modelService.saveAll();
    }

    @Test
    public void shouldExecuteQueryOnReadOnlyDataSource()
    {

        // given
        final String dataSource = "F";

        // when
        final SqlSearchResultData actual = hacFlexibleSearchFacade.executeFlexibleSearchQuery(EXAMPLE_FS_QUERY,
                employee, new Locale("en"), 200, false, dataSource);

        // then
        assertThat(actual.getDataSourceId()).isEqualToIgnoringCase(dataSource);
    }

	@After
	public void tearDown()
	{
		flexibleSearchReadOnlyDataSource.switchBackToDefault();
	}

	@Test
	public void shouldExecuteQueryOnMasterDataSource()
    {

		// given
		final String dataSource = "master";

		// when
		final SqlSearchResultData actual = hacFlexibleSearchFacade.executeFlexibleSearchQuery(EXAMPLE_FS_QUERY,
                employee, new Locale("en"), 200, false, dataSource);

		// then
        assertThat(actual.getDataSourceId()).isEqualToIgnoringCase(dataSource);
	}

	@Test
    public void shouldReturnFDataSourceAsDefault()
    {
        final String expected = Config.getParameter(ReadOnlyConditionsHelper.PARAM_FS_READ_ONLY_DATASOURCE);

        // when
        final String actual = hacFlexibleSearchFacade.getDefaultDataSource();

        // then
        assertThat(actual).isEqualToIgnoringCase(expected);
    }

    @Test
    public void shouldReturnAllDataSources()
    {
        // given

        List<String> expected = new ArrayList<>(hacDatabaseFacade.databaseInfo().keySet());

        // when
        final List<String> actual = hacFlexibleSearchFacade.getAllDataSources();
        Collections.sort(expected);
        Collections.sort(actual);

        // then
        assertThat(actual).isEqualTo(expected);
    }
}