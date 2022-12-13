package de.hybris.platform.servicelayer.interceptor;

import static org.fest.assertions.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.bootstrap.config.tenants.Tenant;
import de.hybris.platform.commons.enums.DocumentTypeEnum;
import de.hybris.platform.commons.model.CustomOrder2XMLModel;
import de.hybris.platform.commons.model.DocumentModel;
import de.hybris.platform.commons.model.FormatModel;
import de.hybris.platform.commons.model.ItemFormatterModel;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.jdbcwrapper.JdbcTestSupport;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.Config;
import de.hybris.platform.util.config.PropertyActionReader;

import java.util.UUID;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Test;

import jersey.repackaged.com.google.common.collect.ImmutableMap;

@IntegrationTest
public class PartOfModelRegisterForRemoveInterceptorAndItemIntegrationTest extends ServicelayerTransactionalBaseTest
{
	@Resource
	ModelService modelService;

	@Resource
	FlexibleSearchService flexibleSearchService;

	private final PropertyConfigSwitcher partOfSwitcher = new PropertyConfigSwitcher("relations.partof.suppressRemoveOnJalo");
	private final PropertyConfigSwitcher allDocumentsSwitcher = new PropertyConfigSwitcher("allDocuments.partof.removal.disabled.for.type.Title");
	private final JdbcTestSupport.JdbcStatistics stats = JdbcTestSupport.createNewJdbcStatistics();

	@After
	public void cleanUp()
	{
		partOfSwitcher.switchBackToDefault();
		allDocumentsSwitcher.switchBackToDefault();
		stats.detach();
	}

	@Test
	public void shouldDeleteDocumentTest()
	{
		allDocumentsSwitcher.switchToValue("false");
		partOfSwitcher.switchToValue("false");
		PropertyActionReader.getPropertyActionReader().clearConfiguration();

		final DocumentModel documentModel = createTitleWithDocument();
		final TitleModel title = (TitleModel) documentModel.getSourceItem();
		final PK documentPk = documentModel.getPk();
		stats.attachToCurrentThread();
		modelService.remove(title);

		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery("select {pk} from {Media} where {pk}=?value", ImmutableMap.of("value", documentPk));
		assertThat(flexibleSearchService.search(fQuery).getResult()).hasSize(0);
		stats.assertThat().selectStatements().filteredOn(s -> s.contains(getMediasTableName())).size().isEqualTo(4);
	}

	@Test
	public void shouldNotDeleteDocumentTest()
	{
		partOfSwitcher.switchToValue("true");
		allDocumentsSwitcher.switchToValue("true");
		PropertyActionReader.getPropertyActionReader().clearConfiguration();

		final DocumentModel documentModel = createTitleWithDocument();
		final TitleModel title = (TitleModel) documentModel.getSourceItem();
		final PK documentPk = documentModel.getPk();
		stats.attachToCurrentThread();
		modelService.remove(title);

		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery("select {pk} from {Media} where {pk}=?value", ImmutableMap.of("value", documentPk));
		assertThat(flexibleSearchService.search(fQuery).getResult()).hasSize(1);
		stats.assertThat().selectStatements().filteredOn(s -> s.contains(getMediasTableName())).size().isEqualTo(1);
	}

	@Test
	public void anotherShouldNotDeleteDocumentTest()
	{
		partOfSwitcher.switchToValue("false");
		allDocumentsSwitcher.switchToValue("true");
		PropertyActionReader.getPropertyActionReader().clearConfiguration();

		final DocumentModel documentModel = createTitleWithDocument();
		final TitleModel title = (TitleModel) documentModel.getSourceItem();
		final PK documentPk = documentModel.getPk();
		stats.attachToCurrentThread();
		modelService.remove(title);

		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery("select {pk} from {Media} where {pk}=?value", ImmutableMap.of("value", documentPk));
		assertThat(flexibleSearchService.search(fQuery).getResult()).hasSize(1);
	}

	@Test
	public void yetAnotherShouldNotDeleteDocumentTest()
	{
		partOfSwitcher.switchToValue("true");
		allDocumentsSwitcher.switchToValue("false");
		PropertyActionReader.getPropertyActionReader().clearConfiguration();

		final DocumentModel documentModel = createTitleWithDocument();
		final TitleModel title = (TitleModel) documentModel.getSourceItem();
		final PK documentPk = documentModel.getPk();
		stats.attachToCurrentThread();
		modelService.remove(title);

		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery("select {pk} from {Media} where {pk}=?value", ImmutableMap.of("value", documentPk));
		assertThat(flexibleSearchService.search(fQuery).getResult()).hasSize(1);
	}

	protected DocumentModel createTitleWithDocument()
	{
		final TitleModel titleModel = modelService.create(TitleModel.class);
		titleModel.setCode(UUID.randomUUID().toString());

		final ItemFormatterModel ifm = modelService.create(CustomOrder2XMLModel.class);
		ifm.setCode(UUID.randomUUID().toString());
		ifm.setOutputMimeType("application/pdf");

		final FormatModel formatModel = modelService.create(FormatModel.class);
		formatModel.setCode(UUID.randomUUID().toString());
		formatModel.setDocumentType(DocumentTypeEnum.PDF);
		formatModel.setInitial(ifm);

		final DocumentModel doc = modelService.create(DocumentModel.class);
		doc.setCode(UUID.randomUUID().toString());
		doc.setFormat(formatModel);

		doc.setSourceItem(titleModel);
		modelService.saveAll();
		return doc;
	}
	
	private String getMediasTableName()
	{
		return Config.getString("db.tableprefix", "") + "medias";
	}
}
