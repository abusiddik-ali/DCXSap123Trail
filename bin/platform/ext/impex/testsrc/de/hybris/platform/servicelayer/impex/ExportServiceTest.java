/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.impex;

import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertTrue;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertNotSame;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.product.UnitModel;
import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.core.model.user.UserGroupModel;
import de.hybris.platform.impex.enums.ImpExValidationModeEnum;
import de.hybris.platform.impex.jalo.exp.Export;
import de.hybris.platform.impex.jalo.exp.ImpExExportMedia;
import de.hybris.platform.impex.jalo.exp.converter.DefaultExportResultHandler;
import de.hybris.platform.impex.model.cronjob.ImpExExportCronJobModel;
import de.hybris.platform.jalo.JaloBusinessException;
import de.hybris.platform.jalo.product.ProductManager;
import de.hybris.platform.servicelayer.ServicelayerTransactionalTest;
import de.hybris.platform.servicelayer.impex.impl.StreamBasedImpExResource;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.locking.ItemLockingService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.util.CSVConstants;
import de.hybris.platform.util.Utilities;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.zip.ZipEntry;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class ExportServiceTest extends ServicelayerTransactionalTest
{
	@Resource
	private ExportService exportService;
	@Resource
	private ModelService modelService;
	@Resource
	private UserService userService;
	@Resource
	private ItemLockingService itemLockingService;

	private Set<PrincipalGroupModel> oldGroups;

	@Before
	public void setUp() throws Exception
	{
		createCoreData();
		createDefaultCatalog();
	}

	@After
	public void cleanUp()
	{
		if (userService.getCurrentUser() != null)
		{
			userService.getCurrentUser().setGroups(oldGroups);
			modelService.save(userService.getCurrentUser());
		}
	}

	@Test
	public void testCreate() throws InterceptorException
	{
		final ImpExExportCronJobModel cronJob = modelService.create(ImpExExportCronJobModel._TYPECODE);
		modelService.initDefaults(cronJob);
		assertNotNull("no job model set to created cronjob model", cronJob.getJob());

		assertNotNull("no code set to created job model", cronJob.getJob().getCode());
		modelService.save(cronJob.getJob());
		modelService.save(cronJob);

		assertNotNull("no code set to created cronjob model", cronJob.getCode());
	}

	@Test
	public void testInit() throws InterceptorException
	{
		final ImpExExportCronJobModel cronJob = new ImpExExportCronJobModel();
		modelService.initDefaults(cronJob);

		assertNotNull("no job model set to created cronjob model", cronJob.getJob());

		modelService.save(cronJob.getJob());
		modelService.save(cronJob);

		assertNotNull("no code set to created cronjob model", cronJob.getCode());
	}

	@Test
	public void testExportByResource() throws IOException, JaloBusinessException
	{
		final ImpExResource res = new StreamBasedImpExResource(getClass().getResourceAsStream(
				"/impex/testfiles/productexportscript2.impex"), CSVConstants.HYBRIS_ENCODING);
		final ExportResult result = exportService.exportData(res);

		assertNotNull("", result);
		assertTrue("", result.isSuccessful());
		assertFalse("", result.isError());
		assertTrue("", result.isFinished());

		assertNotNull("", result.getExport());
		assertNotNull("", result.getExportedData());

		try (final DefaultExportResultHandler handler = new DefaultExportResultHandler())
		{
			handler.setExport(modelService.getSource(result.getExport()));

			final List<ZipEntry> entries = handler
					.getZipEntries(modelService.getSource(result.getExportedData()));
			StringBuilder resultMesg = new StringBuilder();

			for (final ZipEntry entry : entries)
			{
				if (entry.getName().equals("Product.csv"))
				{
					resultMesg = handler.getDataContent(entry);
				}
			}

			assertNotSame("testProduct0 was not exported", Integer.valueOf(-1),
					Integer.valueOf(resultMesg.indexOf("testProduct0")));
			assertNotSame("testProduct1 was not exported", Integer.valueOf(-1),
					Integer.valueOf(resultMesg.indexOf("testProduct1")));
			assertNotSame("testProduct2 was not exported", Integer.valueOf(-1),
					Integer.valueOf(resultMesg.indexOf("testProduct2")));
			assertNotSame("testProduct3 was not exported", Integer.valueOf(-1),
					Integer.valueOf(resultMesg.indexOf("testProduct3")));
			assertNotSame("testProduct4 was not exported", Integer.valueOf(-1),
					Integer.valueOf(resultMesg.indexOf("testProduct4")));
		}
	}

	@Test
	public void testExportByConfig() throws IOException, JaloBusinessException
	{
		final int tempFilesBefore = countProcessLogsTempFiles();
		final ImpExResource res = new StreamBasedImpExResource(getClass().getResourceAsStream(
				"/impex/testfiles/productexportscript.impex"), CSVConstants.HYBRIS_ENCODING);
		res.getMedia().setFieldSeparator(Character.valueOf('|'));
		modelService.save(res.getMedia());
		final ExportConfig config = new ExportConfig();
		config.setScript(res);
		config.setFieldSeparator('|');
		final ExportResult result = exportService.exportData(config);

		assertNotNull("", result);
		assertTrue("", result.isSuccessful());
		assertFalse("", result.isError());
		assertTrue("", result.isFinished());

		try (final DefaultExportResultHandler handler = new DefaultExportResultHandler())
		{
			handler.setExport((Export) modelService.getSource(result.getExport()));
			final List<ZipEntry> entries = handler
					.getZipEntries((ImpExExportMedia) modelService.getSource(result.getExportedData()));
			StringBuilder resultMsg = new StringBuilder();

			for (final ZipEntry entry : entries)
			{
				if (entry.getName().equals("Product.csv"))
				{
					resultMsg = handler.getDataContent(entry);
				}
			}

			assertNotSame("testProduct0 was not exported", Integer.valueOf(-1),
					Integer.valueOf(resultMsg.indexOf("testProduct0")));
			assertNotSame("testProduct1 was not exported", Integer.valueOf(-1),
					Integer.valueOf(resultMsg.indexOf("testProduct1")));
			assertNotSame("testProduct2 was not exported", Integer.valueOf(-1),
					Integer.valueOf(resultMsg.indexOf("testProduct2")));
			assertNotSame("testProduct3 was not exported", Integer.valueOf(-1),
					Integer.valueOf(resultMsg.indexOf("testProduct3")));
			assertNotSame("testProduct4 was not exported", Integer.valueOf(-1),
					Integer.valueOf(resultMsg.indexOf("testProduct4")));

			final int tempFilesAfter = countProcessLogsTempFiles();
			assertThat(tempFilesAfter).isEqualTo(tempFilesBefore);
		}
	}

	@Test
	public void testExportByConfigWithSealedObject()
	{
		prepareUserForLocking();
		final UnitModel unit = modelService.create(UnitModel._TYPECODE);
		unit.setCode("unit-01");
		unit.setUnitType("unit-type");
		final ProductModel product = modelService
				.get(ProductManager.getInstance().getProductsByCode("testProduct0").iterator().next());
		product.setUnit(unit);

		modelService.saveAll(unit, product);
		itemLockingService.lock(product);
		modelService.remove(unit);

		final ImpExResource res = new StreamBasedImpExResource(
				getClass().getResourceAsStream("/impex/testfiles/productexportscript4.impex"), CSVConstants.HYBRIS_ENCODING);
		res.getMedia().setFieldSeparator(Character.valueOf('|'));
		modelService.save(res.getMedia());
		final ExportConfig config = new ExportConfig();
		config.setScript(res);
		config.setFieldSeparator('|');
		final ExportResult result = exportService.exportData(config);

		assertThat(result.isSuccessful()).isTrue();
		assertThat(result.isError()).isFalse();
		assertThat(result.isFinished()).isTrue();
	}

	private void prepareUserForLocking()
	{
		final PrincipalGroupModel itemLockingGroup = modelService.create(UserGroupModel.class);
		itemLockingGroup.setUid("itemLockingGroup");
		modelService.save(itemLockingGroup);

		oldGroups = userService.getCurrentUser().getGroups();
		final Set<PrincipalGroupModel> groupsWithItemLockingGroup = new HashSet<>(oldGroups);

		groupsWithItemLockingGroup.add(itemLockingGroup);

		userService.getCurrentUser().setGroups(groupsWithItemLockingGroup);
	}

	private int countProcessLogsTempFiles()
	{
		final File platformTempDir = Utilities.getPlatformTempDir();
		return platformTempDir.listFiles((dir, name) -> name.startsWith("ImpExZip") && name.endsWith("zip")).length;
	}

	@Test
	public void invalidExportScriptShouldValidateNotOk()
	{
		final String invalidExportScript = "insert_update Catalog;id[unique=true];activeCata22222logVersion(catalog(id),version)\n"
				+ "\"#% impex.exportItems(\"\"SELECT {C:pk} FROM {Catalog as C} WHERE {C:id}='$catalog'\"\", Collections.EMPTY_MAP, Collections.singletonList( Item.class ), true, true, -1, -1 );\"\n";
		final ImpExValidationResult result = exportService.validateExportScript(invalidExportScript,
				ImpExValidationModeEnum.EXPORT_REIMPORT_STRICT);

		assertThat(result.isSuccessful()).isFalse();
		assertThat(result.getFailureCause()).isNotEmpty();
	}

	@Test
	public void validExportScriptShouldValidateOk()
	{
		final String validExportScript = "insert_update Catalog;id[unique=true];activeCatalogVersion(catalog(id),version)\n"
				+ "\"#% impex.exportItems(\"\"SELECT {C:pk} FROM {Catalog as C} WHERE {C:id}='$catalog'\"\", Collections.EMPTY_MAP, Collections.singletonList( Item.class ), true, true, -1, -1 );\"\n";

		final ImpExValidationResult result = exportService.validateExportScript(validExportScript,
				ImpExValidationModeEnum.EXPORT_REIMPORT_STRICT);

		assertThat(result.isSuccessful()).isTrue();
		assertThat(result.getFailureCause()).isNull();
	}


}
