/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */

package de.hybris.platform.catalog.synchronization;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.enums.ClassificationAttributeTypeEnum;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.catalog.model.ItemSyncTimestampModel;
import de.hybris.platform.catalog.model.ProductFeatureModel;
import de.hybris.platform.catalog.model.classification.ClassAttributeAssignmentModel;
import de.hybris.platform.catalog.model.classification.ClassificationAttributeModel;
import de.hybris.platform.catalog.model.classification.ClassificationClassModel;
import de.hybris.platform.catalog.model.classification.ClassificationSystemModel;
import de.hybris.platform.catalog.model.classification.ClassificationSystemVersionModel;
import de.hybris.platform.catalog.model.synchronization.CatalogVersionSyncJobModel;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.type.TypeService;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class StaleTSSyncTest extends ServicelayerBaseTest
{
	@Resource
	private ModelService modelService;

	@Resource
	private TypeService typeService;

	@Resource
	private CatalogSynchronizationService catalogSynchronizationService;

	private CatalogModel catalog;

	private CatalogVersionModel staged, online;

	private ClassificationSystemVersionModel clSys;

	private ClassAttributeAssignmentModel clAttribute;

	private ProductModel product;

	private ProductFeatureModel one, two, three;

	private CatalogVersionSyncJobModel syncJob;

	@Before
	public void setup()
	{
		setupCatalog();
		createSyncJob();
		modelService.saveAll();
	}

	@Test
	public void testSyncDefaultBehaviour()
	{
		catalogSynchronizationService.synchronizeFully(staged, online);

		final ProductModel productOnline = catalogSynchronizationService.getSynchronizationTargetFor(syncJob, product);
		assertNotNull(productOnline);

		final List<ProductFeatureModel> featuresOnline = productOnline.getFeatures();
		assertNotNull(featuresOnline);
		assertEquals(3, featuresOnline.size());

		final ProductFeatureModel oneOnline = catalogSynchronizationService.getSynchronizationTargetFor(syncJob, one);
		assertNotNull(oneOnline);
		assertTrue(featuresOnline.contains(oneOnline));

		final ProductFeatureModel twoOnline = catalogSynchronizationService.getSynchronizationTargetFor(syncJob, two);
		assertNotNull(twoOnline);
		assertTrue(featuresOnline.contains(twoOnline));

		final ProductFeatureModel threeOnline = catalogSynchronizationService.getSynchronizationTargetFor(syncJob, three);
		assertNotNull(threeOnline);
		assertTrue(featuresOnline.contains(threeOnline));


		final ItemSyncTimestampModel tsOne = catalogSynchronizationService.getSynchronizationSourceTimestampFor(syncJob, one);
		assertNotNull(tsOne);
		assertEquals(one, tsOne.getSourceItem());
		assertEquals(oneOnline, tsOne.getTargetItem());

		modelService.remove(oneOnline);

		// need a change to trigger re-sync
		product.setModifiedtime(new Date(productOnline.getModifiedtime().getTime() + 5000));
		modelService.save(product);

		assertNull(catalogSynchronizationService.getSynchronizationSourceTimestampFor(syncJob, one));
		//		modelService.refresh(tsOne);
		modelService.isRemoved(tsOne);

		//		modelService.refresh(oneOnline);
		modelService.isRemoved(oneOnline);

		modelService.refresh(productOnline);
		assertFalse(productOnline.getFeatures().contains(oneOnline));


		catalogSynchronizationService.synchronizeFully(staged, online);

		modelService.refresh(productOnline);

		final List<ProductFeatureModel> featuresOnlineAgain = productOnline.getFeatures();
		assertNotNull(featuresOnline);
		assertEquals(3, featuresOnline.size());


		final ProductFeatureModel oneOnlineAgain = catalogSynchronizationService.getSynchronizationTargetFor(syncJob, one);
		assertNotNull(oneOnlineAgain);
		assertTrue(featuresOnlineAgain.contains(oneOnlineAgain));

	}

	@Test
	public void testSyncWithStaleTimestamp()
	{
		catalogSynchronizationService.synchronizeFully(staged, online);

		final ProductModel productOnline = catalogSynchronizationService.getSynchronizationTargetFor(syncJob, product);
		assertNotNull(productOnline);

		final List<ProductFeatureModel> featuresOnline = productOnline.getFeatures();
		assertNotNull(featuresOnline);
		assertEquals(3, featuresOnline.size());

		final ProductFeatureModel oneOnline = catalogSynchronizationService.getSynchronizationTargetFor(syncJob, one);
		assertNotNull(oneOnline);
		assertTrue(featuresOnline.contains(oneOnline));

		final ProductFeatureModel twoOnline = catalogSynchronizationService.getSynchronizationTargetFor(syncJob, two);
		assertNotNull(twoOnline);
		assertTrue(featuresOnline.contains(twoOnline));

		final ProductFeatureModel threeOnline = catalogSynchronizationService.getSynchronizationTargetFor(syncJob, three);
		assertNotNull(threeOnline);
		assertTrue(featuresOnline.contains(threeOnline));

		final ItemSyncTimestampModel tsOne = catalogSynchronizationService.getSynchronizationSourceTimestampFor(syncJob, one);
		assertNotNull(tsOne);
		assertEquals(one, tsOne.getSourceItem());
		assertEquals(oneOnline, tsOne.getTargetItem());

		// need a change to trigger re-sync
		product.setModifiedtime(new Date(productOnline.getModifiedtime().getTime() + 5000));
		modelService.save(product);

		// now remove feature database record directly
		removeViaJDBC(oneOnline);

		// we MUST clear the complete cache. otherwise there will be old query results popping up later!
		Registry.getCurrentTenantNoFallback().getCache().clear();

		assertFalse(existsViaJDBC(oneOnline));
		assertTrue(existsViaJDBC(tsOne));

		catalogSynchronizationService.synchronizeFully(staged, online);

		modelService.refresh(productOnline);
		modelService.refresh(tsOne);

		final List<ProductFeatureModel> featuresOnlineAgain = productOnline.getFeatures();
		assertNotNull(featuresOnline);
		assertEquals(3, featuresOnline.size());

		final ItemSyncTimestampModel tsOneAgain = catalogSynchronizationService.getSynchronizationSourceTimestampFor(syncJob,
				one);
		assertNotNull(tsOneAgain);
		assertEquals(one, tsOneAgain.getSourceItem());
		assertEquals(tsOne, tsOneAgain);
		assertNotNull(tsOneAgain.getTargetItem());
		final ProductFeatureModel oneOnlineAgainFromTS = (ProductFeatureModel) tsOneAgain.getTargetItem();
		assertEquals(one.getValue(), oneOnlineAgainFromTS.getValue());
		assertEquals(one.getValuePosition(), oneOnlineAgainFromTS.getValuePosition());

		final ProductFeatureModel oneOnlineAgain = catalogSynchronizationService.getSynchronizationTargetFor(syncJob, one);
		assertNotNull(oneOnlineAgain);
		assertNotEquals(oneOnlineAgain, oneOnline);
		assertTrue(featuresOnlineAgain.contains(oneOnlineAgain));
		assertTrue(modelService.isRemoved(oneOnline));

		assertEquals(oneOnlineAgain, tsOne.getTargetItem());

	}

	void removeViaJDBC(final ProductFeatureModel feature)
	{
		final String tableName = typeService.getComposedType(ProductFeatureModel.class).getTable();

		try (final Connection con = Registry.getCurrentTenantNoFallback().getDataSource().getConnection();
		     final Statement stmt = con.createStatement())
		{
			stmt.executeUpdate("DELETE FROM " + tableName + " WHERE PK=" + feature.getPk().getLongValue());
		}
		catch (final SQLException e)
		{
			fail("Error removing feature " + feature + " via JDBC: " + e);
		}
	}

	boolean existsViaJDBC(final ItemModel model)
	{
		final String tableName = typeService.getComposedType(model.getClass()).getTable();

		try (final Connection con = Registry.getCurrentTenantNoFallback().getDataSource().getConnection();
		     final Statement stmt = con.createStatement();
		     final ResultSet rs = stmt.executeQuery(
				     "SELECT count(*) FROM " + tableName + " WHERE PK=" + model.getPk().getLongValue()))
		{
			return rs.next() && rs.getInt(1) > 0;
		}
		catch (final SQLException e)
		{
			fail("Error checking existence of item " + model + " via JDBC: " + e);
		}
		return false;
	}


	void setupCatalog()
	{
		// catalog
		catalog = modelService.create(CatalogModel.class);
		catalog.setId("Catalog");

		staged = modelService.create(CatalogVersionModel.class);
		staged.setCatalog(catalog);
		staged.setVersion("staged");

		online = modelService.create(CatalogVersionModel.class);
		online.setCatalog(catalog);
		online.setVersion("online");

		catalog.setActiveCatalogVersion(online);

		// classification
		final ClassificationSystemModel cs = modelService.create(ClassificationSystemModel.class);
		cs.setId("ClassificationSystem");

		clSys = modelService.create(ClassificationSystemVersionModel.class);
		clSys.setVersion("1.0");
		clSys.setCatalog(cs);

		cs.setActiveCatalogVersion(clSys);

		final ClassificationAttributeModel attr = modelService.create(ClassificationAttributeModel.class);
		attr.setSystemVersion(clSys);
		attr.setCode("attribute");

		final ClassificationClassModel clClass = modelService.create(ClassificationClassModel.class);
		clClass.setCatalogVersion(clSys);
		clClass.setCode("ClClass");

		clAttribute = modelService.create(ClassAttributeAssignmentModel.class);
		clAttribute.setSystemVersion(clSys);
		clAttribute.setClassificationClass(clClass);
		clAttribute.setClassificationAttribute(attr);
		clAttribute.setAttributeType(ClassificationAttributeTypeEnum.STRING);
		clAttribute.setMultiValued(true);

		// product
		product = modelService.create(ProductModel.class);
		product.setCatalogVersion(staged);
		product.setCode("Product");
		product.setSupercategories(Collections.singletonList(clAttribute.getClassificationClass()));

		one = modelService.create(ProductFeatureModel.class);
		one.setProduct(product);
		one.setClassificationAttributeAssignment(clAttribute);
		one.setValue("one");
		one.setValuePosition(0);

		two = modelService.create(ProductFeatureModel.class);
		two.setProduct(product);
		two.setClassificationAttributeAssignment(clAttribute);
		two.setValue("two");
		two.setValuePosition(1);

		three = modelService.create(ProductFeatureModel.class);
		three.setProduct(product);
		three.setClassificationAttributeAssignment(clAttribute);
		three.setValue("three");
		three.setValuePosition(2);

		product.setFeatures(Arrays.asList(one, two, three));
	}


	void createSyncJob()
	{
		syncJob = modelService.create(CatalogVersionSyncJobModel.class);
		syncJob.setCode("SyncJob");
		syncJob.setSourceVersion(staged);
		syncJob.setTargetVersion(online);
		syncJob.setMaxThreads(5);
		syncJob.setCreateNewItems(true);
		syncJob.setRemoveMissingItems(true);
	}
}
