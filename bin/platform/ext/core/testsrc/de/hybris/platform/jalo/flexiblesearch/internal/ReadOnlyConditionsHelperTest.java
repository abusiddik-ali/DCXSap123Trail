/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.jalo.flexiblesearch.internal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Tenant;
import de.hybris.platform.jalo.SessionContext;
import de.hybris.platform.jalo.flexiblesearch.AbstractSwitchingDataSourceTest;
import de.hybris.platform.jalo.flexiblesearch.hints.Hint;
import de.hybris.platform.jalo.flexiblesearch.hints.impl.FlexibleSearchHints;
import de.hybris.platform.jalo.flexiblesearch.hints.impl.FlexibleSearchHints.CategorizedQueryHint;
import de.hybris.platform.jdbcwrapper.HybrisDataSource;
import de.hybris.platform.testframework.BulkPropertyConfigSwitcher;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.tx.Transaction;
import de.hybris.platform.tx.TransactionBody;
import de.hybris.platform.util.Config;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.sql.DataSource;

import org.apache.commons.lang3.RandomStringUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class ReadOnlyConditionsHelperTest extends AbstractSwitchingDataSourceTest
{
	private final PropertyConfigSwitcher readOnlyDataSourceProperty = new PropertyConfigSwitcher(
			ReadOnlyConditionsHelper.PARAM_FS_READ_ONLY_DATASOURCE);

	private final BulkPropertyConfigSwitcher config = new BulkPropertyConfigSwitcher();
	private SessionContext localCtx;

	@Override
	@Before
	public void setUp() throws Exception
	{
		super.setUp();

		localCtx = jaloSession.createLocalSessionContext();
		CategorizedQueryHint.clearCachedConfig();
	}

	@Override
	@After
	public void tearDown()
	{
		super.tearDown();
		readOnlyDataSourceProperty.switchBackToDefault();
		config.switchAllBack();

		jaloSession.removeLocalSessionContext();
		CategorizedQueryHint.clearCachedConfig();
	}

	@Test
	public void shouldNotReturnReadOnlyDataSourceIfConfigurationIsEmpty()
	{
		readOnlyDataSourceProperty.switchToValue(" ");

		final Optional<HybrisDataSource> readOnlyDataSource = new ReadOnlyConditionsHelper().getReadOnlyDataSource(tenant);

		assertThat(readOnlyDataSource).isEmpty();
	}

	@Test
	public void shouldNotReturnReadOnlyDataSourceIfConfiguredDataSourceDoesNotExist()
	{
		final String configuredReadOnlyDataSource = RandomStringUtils.randomAlphabetic(5).toLowerCase(Locale.ROOT);
		assertThat(configuredReadOnlyDataSource).isNotIn(tenant.getAllDataSourceIDs());

		readOnlyDataSourceProperty.switchToValue(configuredReadOnlyDataSource);

		final Optional<HybrisDataSource> readOnlyDataSource = new ReadOnlyConditionsHelper().getReadOnlyDataSource(tenant);

		assertThat(readOnlyDataSource).isEmpty();
	}

	@Test
	public void shouldReturnReadOnlyDataSourceIfConfiguredToExistingSlaveDataSource()
	{
		final DataSource defaultDataSource = tenant.getDataSource();
		readOnlyDataSourceProperty.switchToValue("d");

		final Optional<HybrisDataSource> readOnlyDataSource = new ReadOnlyConditionsHelper().getReadOnlyDataSource(tenant);

		assertThat(readOnlyDataSource).isNotEmpty();
		assertThat(readOnlyDataSource.get()).isNotEqualTo(defaultDataSource).extracting(HybrisDataSource::getID).contains("d");
	}

	private void testCouldUseReadOnlyDataSource(final Object sessionAttributeValue, final List<Hint> hints,
	                                            final boolean expectedResult)
	{
		assertThat(Transaction.current().isRunning()).isFalse();
		assertThat(tenant.isAlternativeMasterDataSource()).isFalse();
		assertThat(tenant.isSlaveDataSource()).isFalse();

		if (sessionAttributeValue != null)
		{
			localCtx.setAttribute(ReadOnlyConditionsHelper.CTX_ENABLE_FS_ON_READ_REPLICA, sessionAttributeValue);
		}
		else
		{
			localCtx.removeAttribute(ReadOnlyConditionsHelper.CTX_ENABLE_FS_ON_READ_REPLICA);
		}

		final boolean useReadOnlyDataSource = new ReadOnlyConditionsHelper().couldUseReadOnlyDataSource(tenant, hints);

		assertThat(useReadOnlyDataSource).isEqualTo(expectedResult);
	}

	@Test
	public void shouldAllowUsingReadOnlyDataSourceWhenArgumentInSessionContextIsTrueAsString()
	{
		testCouldUseReadOnlyDataSource("true", List.of(), true);
	}

	@Test
	public void shouldAllowUsingReadOnlyDataSourceWhenArgumentInSessionContextIsTrue()
	{
		testCouldUseReadOnlyDataSource(Boolean.TRUE, List.of(), true);
	}

	@Test
	public void shouldNotAllowUsingReadOnlyDataSourceWhenArgumentInSessionContextIsFalse()
	{
		testCouldUseReadOnlyDataSource(Boolean.FALSE, List.of(), false);
	}

	@Test
	public void shouldNotAllowUsingReadOnlyDataSourceWhenArgumentInSessionContextIsNotAValidBoolean()
	{
		testCouldUseReadOnlyDataSource("thisIsNotABoolean", List.of(), false);
	}

	@Test
	public void shouldNotAllowUsingReadOnlyDataSourceWhenNoArgumentInSessionContext()
	{
		testCouldUseReadOnlyDataSource(null, List.of(), false);
	}


	@Test
	public void shouldNotAllowUsingReadOnlyDataSourceWhenWithinActiveTx() throws Exception
	{

		assertThat(tenant.isAlternativeMasterDataSource()).isFalse();
		assertThat(tenant.isSlaveDataSource()).isFalse();
		localCtx.setAttribute(ReadOnlyConditionsHelper.CTX_ENABLE_FS_ON_READ_REPLICA, Boolean.TRUE);

		final ReadOnlyConditionsHelper readOnlyConditionsHelper = new ReadOnlyConditionsHelper();
		final boolean useReadOnlyDataSource = (boolean) Transaction.current().execute(new TransactionBody()
		{
			@Override
			public Boolean execute()
			{
				return readOnlyConditionsHelper.couldUseReadOnlyDataSource(tenant, List.of());
			}
		});

		assertThat(useReadOnlyDataSource).isFalse();
	}

	@Test
	public void shouldNotAllowUsingReadOnlyDataSourceWhenAlternativeDataSourceIsActive()
	{
		assertThat(Transaction.current().isRunning()).isFalse();
		localCtx.setAttribute(ReadOnlyConditionsHelper.CTX_ENABLE_FS_ON_READ_REPLICA, Boolean.TRUE);

		final AtomicBoolean useReadOnlyDataSource = new AtomicBoolean();
		final ReadOnlyConditionsHelper readOnlyConditionsHelper = new ReadOnlyConditionsHelper();

		doWithActivatedAlternativeDataSource("alt1", () -> {
			useReadOnlyDataSource.set(readOnlyConditionsHelper.couldUseReadOnlyDataSource(tenant, List.of()));
		});

		assertThat(useReadOnlyDataSource.get()).isFalse();
	}

	@Test
	public void shouldNotAllowUsingReadOnlyDataSourceWhenSlaveDataSourceIsActive()
	{
		assertThat(Transaction.current().isRunning()).isFalse();
		localCtx.setAttribute(ReadOnlyConditionsHelper.CTX_ENABLE_FS_ON_READ_REPLICA, Boolean.TRUE);

		final AtomicBoolean useReadOnlyDataSource = new AtomicBoolean();

		doWithActivatedSlaveDataSource("a", () -> {
			useReadOnlyDataSource.set(new ReadOnlyConditionsHelper().couldUseReadOnlyDataSource(tenant, List.of()));
		});

		assertThat(useReadOnlyDataSource.get()).isFalse();
	}

	@Test
	public void shouldAllowUsingReadOnlyDataSourceWhenAtLeastOneHintIsTrueAndNoFalseHints()
	{
		setFlexibleSearchCategoryUseReadOnlyDataSource("cat1", null);
		setFlexibleSearchCategoryUseReadOnlyDataSource("cat2", true);
		setFlexibleSearchCategoryUseReadOnlyDataSource("cat3", null);

		final List<Hint> hints = new ArrayList<>();
		hints.add(FlexibleSearchHints.categorizedQuery("cat1"));
		hints.add(FlexibleSearchHints.categorizedQuery("cat2"));
		hints.add(FlexibleSearchHints.categorizedQuery("cat3"));

		testCouldUseReadOnlyDataSource(null, hints, true);
	}

	@Test
	public void shouldNotAllowUsingReadOnlyDataSourceWhenNoTrueHints()
	{
		final List<Hint> hints = new ArrayList<>();
		hints.add(FlexibleSearchHints.categorizedQuery("cat1"));
		hints.add(FlexibleSearchHints.categorizedQuery("cat2"));

		testCouldUseReadOnlyDataSource(null, hints, false);
	}


	@Test
	public void shouldAllowUsingReadOnlyDataSourceWhenNoTrueHintsAndArgumentInSessionIsTrue()
	{
		final List<Hint> hints = new ArrayList<>();
		hints.add(FlexibleSearchHints.categorizedQuery("cat1"));
		hints.add(FlexibleSearchHints.categorizedQuery("cat2"));

		testCouldUseReadOnlyDataSource(Boolean.TRUE, hints, true);
	}

	@Test
	public void shouldNotAllowUsingReadOnlyDataSourceWhenAtLeastOneFalseHint()
	{
		setFlexibleSearchCategoryUseReadOnlyDataSource("cat1", null);
		setFlexibleSearchCategoryUseReadOnlyDataSource("cat2", true);
		setFlexibleSearchCategoryUseReadOnlyDataSource("cat3", false);

		final List<Hint> hints = new ArrayList<>();
		hints.add(FlexibleSearchHints.categorizedQuery("cat1"));
		hints.add(FlexibleSearchHints.categorizedQuery("cat2"));
		hints.add(FlexibleSearchHints.categorizedQuery("cat3"));

		testCouldUseReadOnlyDataSource(null, hints, false);
	}

	@Test
	public void shouldNotAllowUsingReadOnlyDataSourceWhenAtLeastOneFalseHintAndArgumentInSessionIsTrue()
	{
		setFlexibleSearchCategoryUseReadOnlyDataSource("cat1", null);
		setFlexibleSearchCategoryUseReadOnlyDataSource("cat2", true);
		setFlexibleSearchCategoryUseReadOnlyDataSource("cat3", false);

		final List<Hint> hints = new ArrayList<>();
		hints.add(FlexibleSearchHints.categorizedQuery("cat1"));
		hints.add(FlexibleSearchHints.categorizedQuery("cat2"));
		hints.add(FlexibleSearchHints.categorizedQuery("cat3"));

		testCouldUseReadOnlyDataSource(Boolean.TRUE, hints, false);
	}


	@Test
	public void shouldAllowUsingReadOnlyDataSourceWhenCategoryIsInvalidArgumentInSessionIsTrue()
	{
		setFlexibleSearchCategoryUseReadOnlyDataSource("cat1", "notValidBool");

		final List<Hint> hints = new ArrayList<>();
		hints.add(FlexibleSearchHints.categorizedQuery("cat1"));

		testCouldUseReadOnlyDataSource(Boolean.TRUE, hints, true);
	}

	@Test
	public void shouldNotAllowUsingReadOnlyDataSourceWhenCategoryIsInvalidArgumentInSessionIsFalse()
	{
		setFlexibleSearchCategoryUseReadOnlyDataSource("cat1", "notValidBool");

		final List<Hint> hints = new ArrayList<>();
		hints.add(FlexibleSearchHints.categorizedQuery("cat1"));

		testCouldUseReadOnlyDataSource(Boolean.FALSE, hints, false);
	}

	@Test
	public void shouldNotAllowUsingReadOnlyDataSourceWhenCategoryIsInvalidArgumentInSessionIsNull()
	{
		setFlexibleSearchCategoryUseReadOnlyDataSource("cat1", "notValidBool");

		final List<Hint> hints = new ArrayList<>();
		hints.add(FlexibleSearchHints.categorizedQuery("cat1"));

		testCouldUseReadOnlyDataSource(null, hints, false);
	}

	@Test
	public void shouldNotAllowUsingReadOnlyDataSourceWhenHintsAreTrueButSessionContextIsFalse()
	{
		setFlexibleSearchCategoryUseReadOnlyDataSource("cat1", true);

		final List<Hint> hints = new ArrayList<>();
		hints.add(FlexibleSearchHints.categorizedQuery("cat1"));

		testCouldUseReadOnlyDataSource(Boolean.FALSE, hints, false);
	}

	@Test
	public void shouldAllowUsingReadOnlyDataSourceWhenHintsAreTrueButSessionContextIsInvalid()
	{
		setFlexibleSearchCategoryUseReadOnlyDataSource("cat1", true);

		final List<Hint> hints = new ArrayList<>();
		hints.add(FlexibleSearchHints.categorizedQuery("cat1"));

		testCouldUseReadOnlyDataSource("thisIsNotABoolean", hints, true);
	}

	@Test
	public void shouldNotUseReadOnlyDataSourceIfItHasDifferentDatabaseProvider()
	{
		assertThat(Transaction.current().isRunning()).isFalse();
		assertThat(tenant.isAlternativeMasterDataSource()).isFalse();
		assertThat(tenant.isSlaveDataSource()).isFalse();
		localCtx.setAttribute(ReadOnlyConditionsHelper.CTX_ENABLE_FS_ON_READ_REPLICA, Boolean.TRUE);

		final HybrisDataSource defaultDataSource = tenant.getDataSource();

		readOnlyDataSourceProperty.switchToValue("mockedSlaveDataSource");
		final Tenant mockTenant = spy(tenant);

		final HybrisDataSource mockedSlaveDataSource = mock(HybrisDataSource.class);
		when(mockedSlaveDataSource.getID()).thenReturn("mockedSlaveDataSource".toLowerCase(Locale.ROOT));

		when(mockTenant.getAllSlaveDataSources()).then(invocationOnMock -> {
			final ArrayList<HybrisDataSource> l = new ArrayList<>(
					((Collection<HybrisDataSource>) invocationOnMock.callRealMethod()));
			l.add(mockedSlaveDataSource);
			return l;
		});

		{
			//verify it would return mocked datasource if the database provider is the same
			when(mockedSlaveDataSource.getDatabaseName()).thenReturn(defaultDataSource.getDatabaseName());
			final Optional<HybrisDataSource> readOnlyDataSource = new ReadOnlyConditionsHelper().getReadOnlyDataSource(
					mockTenant);

			assertThat(readOnlyDataSource).isNotEmpty();
			assertThat(readOnlyDataSource.get()).isEqualTo(mockedSlaveDataSource);
		}

		when(mockedSlaveDataSource.getDatabaseName()).thenReturn(getDifferentDatabaseName(defaultDataSource.getDatabaseName()));
		final Optional<HybrisDataSource> readOnlyDataSource = new ReadOnlyConditionsHelper().getReadOnlyDataSource(mockTenant);

		assertThat(readOnlyDataSource).isEmpty();
	}

	private String getDifferentDatabaseName(final String databaseName)
	{

		for (final Config.DatabaseName name : Config.DatabaseName.values())
		{
			if (!name.getName().equals(databaseName))
			{
				return name.getName();
			}
		}

		throw new IllegalArgumentException("could not find database name different to " + databaseName);
	}

	private void setFlexibleSearchCategoryUseReadOnlyDataSource(final String category, final boolean useReadOnlyDataSource)
	{
		setFlexibleSearchCategoryUseReadOnlyDataSource(category, Boolean.toString(useReadOnlyDataSource));
	}

	private void setFlexibleSearchCategoryUseReadOnlyDataSource(final String category, final String useReadOnlyDataSource)
	{
		config.switchToValue(
				CategorizedQueryHint.PARAM_FS_CATEGORY_PREFIX + category + CategorizedQueryHint.PARAM_FS_CATEGORY_PREFER_RO_DS_SUFFIX,
				useReadOnlyDataSource == null ? " " : useReadOnlyDataSource);
	}
}
