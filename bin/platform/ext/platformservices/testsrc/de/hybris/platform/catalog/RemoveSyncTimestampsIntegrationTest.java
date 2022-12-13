/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog;

import static de.hybris.platform.util.config.PropertyActionReader.getPropertyActionReader;
import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.daos.ItemSyncTimestampDao;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.catalog.model.ItemSyncTimestampModel;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.jdbcwrapper.JdbcTestSupport;
import de.hybris.platform.jdbcwrapper.JdbcTestSupport.JdbcStatistics;
import de.hybris.platform.servicelayer.ServicelayerTransactionalTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.Config;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class RemoveSyncTimestampsIntegrationTest extends ServicelayerTransactionalTest
{
	@Resource
	private ItemSyncTimestampDao itemSyncTimestampDao;

	@Resource
	private ModelService modelService;

	private CatalogVersionModel onlineCatalogVersionModel;
	private CatalogVersionModel stagedCatalogVersionModel;

	private final PropertyConfigSwitcher disallowSyncItemsSwitcher = new PropertyConfigSwitcher(
			"synctimestamp.removal.disabled.for.type.Title");
	private final PropertyConfigSwitcher removalOptimisationSwitcher = new PropertyConfigSwitcher(
			"synctimestamp.query.removal.optimisation");

	private JdbcStatistics stats;

	@Before
	public void setUp() throws Exception
	{
		final CatalogModel catalog = this.modelService.create(CatalogModel.class);
		catalog.setId("my_catalog");
		modelService.save(catalog);

		onlineCatalogVersionModel = modelService.create(CatalogVersionModel.class);
		onlineCatalogVersionModel.setVersion("online_version");
		onlineCatalogVersionModel.setCatalog(catalog);
		modelService.save(onlineCatalogVersionModel);

		stagedCatalogVersionModel = modelService.create(CatalogVersionModel.class);
		stagedCatalogVersionModel.setVersion("staged_version");
		stagedCatalogVersionModel.setCatalog(catalog);
		modelService.save(stagedCatalogVersionModel);

		stats = JdbcTestSupport.createNewJdbcStatistics();
	}

	@After
	public void cleanUp()
	{
		disallowSyncItemsSwitcher.switchBackToDefault();
		removalOptimisationSwitcher.switchBackToDefault();

		stats.detach();
	}

	@Test
	public void testRemoveWithoutOptimisation()
	{
		warmup();
		disallowSyncItemsSwitcher.switchToValue("false");
		getPropertyActionReader().clearConfiguration();

		final TitleModel title1 = createTitle("title-01");
		final TitleModel title2 = createTitle("title-02");

		prepareSyncItems(title1, title2);
		assertThat(itemSyncTimestampDao.findSyncTimestampsByItem(title1, -1)).hasSize(2);

		stats.attachToCurrentThread();
		modelService.remove(title1);
		assertThat(itemSyncTimestampDao.findSyncTimestampsByItem(title1, -1)).hasSize(0);
		stats.assertThat().deleteStatements().filteredOn(s -> s.contains(getItemsynctimestampsTableName())).size().isEqualTo(1);
	}

	@Test
	public void testRemoveWithOptimisation()
	{
		warmup();
		disallowSyncItemsSwitcher.switchToValue("true");
		removalOptimisationSwitcher.switchToValue("true");
		getPropertyActionReader().clearConfiguration();

		final TitleModel title1 = createTitle("title-01");
		final TitleModel title2 = createTitle("title-02");

		prepareSyncItems(title1, title2);

		assertThat(itemSyncTimestampDao.findSyncTimestampsByItem(title1, -1)).hasSize(2);

		stats.attachToCurrentThread();
		modelService.remove(title1);
		assertThat(itemSyncTimestampDao.findSyncTimestampsByItem(title1, -1)).hasSize(2);
		stats.assertThat().deleteStatements().filteredOn(s -> s.contains(getItemsynctimestampsTableName())).size().isEqualTo(0);
	}

	private void warmup()
	{
		final TitleModel title1 = createTitle("title-03");
		final TitleModel title2 = createTitle("title-04");

		prepareSyncItems(title1, title2);

		modelService.remove(title1);
	}

	private void prepareSyncItems(final TitleModel title1, final TitleModel title2)
	{
		final ItemSyncTimestampModel itemSyncTimestampModel1 = modelService.create(ItemSyncTimestampModel._TYPECODE);
		final ItemSyncTimestampModel itemSyncTimestampModel2 = modelService.create(ItemSyncTimestampModel._TYPECODE);
		itemSyncTimestampModel1.setSourceItem(title1);
		itemSyncTimestampModel1.setTargetItem(title1);
		itemSyncTimestampModel1.setSourceVersion(stagedCatalogVersionModel);
		itemSyncTimestampModel1.setTargetVersion(onlineCatalogVersionModel);
		itemSyncTimestampModel2.setSourceItem(title2);
		itemSyncTimestampModel2.setTargetItem(title2);
		itemSyncTimestampModel2.setSourceVersion(stagedCatalogVersionModel);
		itemSyncTimestampModel2.setTargetVersion(onlineCatalogVersionModel);
		modelService.saveAll(itemSyncTimestampModel1, itemSyncTimestampModel2);
	}

	@Test
	public void testQueriesWithOptimisation()
	{
		disallowSyncItemsSwitcher.switchToValue("false");
		removalOptimisationSwitcher.switchToValue("true");
		getPropertyActionReader().clearConfiguration();

		final TitleModel title1 = createTitle("title-01");

		stats.attachToCurrentThread();

		modelService.remove(title1);

		stats.assertThat().selectStatements().filteredOn(s -> s.contains(getItemsynctimestampsTableName())).size().isEqualTo(1);
	}

	@Test
	public void testQueriesWithoutOptimisation()
	{
		disallowSyncItemsSwitcher.switchToValue("false");
		removalOptimisationSwitcher.switchToValue("false");
		getPropertyActionReader().clearConfiguration();

		final TitleModel title1 = createTitle("title-02");

		stats.attachToCurrentThread();

		modelService.remove(title1);

		stats.assertThat().selectStatements().filteredOn(s -> s.contains(getItemsynctimestampsTableName())).size().isEqualTo(2);
	}

	private TitleModel createTitle(final String code)
	{
		final TitleModel title = modelService.create(TitleModel._TYPECODE);
		title.setCode(code);
		modelService.save(title);
		return title;
	}

	private String getItemsynctimestampsTableName()
	{
		return Config.getString("db.tableprefix", "") + "itemsynctimestamps";
	}
}
