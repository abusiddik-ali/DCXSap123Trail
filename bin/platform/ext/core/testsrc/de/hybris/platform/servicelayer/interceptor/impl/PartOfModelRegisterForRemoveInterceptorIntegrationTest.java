/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.interceptor.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.enums.SavedValueEntryType;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.directpersistence.selfhealing.SelfHealingService;
import de.hybris.platform.hmc.model.SavedValueEntryModel;
import de.hybris.platform.hmc.model.SavedValuesModel;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.constants.ServicelayerConstants;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.servicelayer.internal.converter.ConverterRegistry;
import de.hybris.platform.servicelayer.internal.converter.impl.ItemModelConverter;
import de.hybris.platform.servicelayer.internal.model.impl.DefaultModelService;
import de.hybris.platform.servicelayer.internal.model.impl.SourceTransformer;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.type.TypeService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.util.config.ConfigIntf;
import de.hybris.platform.util.config.FallbackConfig;
import de.hybris.platform.util.config.FastHashMapConfig;

import java.util.Collections;
import java.util.Date;
import java.util.Set;

import javax.annotation.Resource;

import org.junit.Test;

@IntegrationTest
public class PartOfModelRegisterForRemoveInterceptorIntegrationTest extends ServicelayerTransactionalBaseTest
{

	@Resource
	ModelService modelService;

	@Resource
	UserService userService;

	@Resource
	TypeService typeService;

	@Resource
	protected I18NService i18nService;
	@Resource
	protected CommonI18NService commonI18NService;
	@Resource
	protected FlexibleSearchService flexibleSearchService;
	@Resource
	protected ConverterRegistry converterRegistry;
	@Resource
	protected SourceTransformer sourceTransformer;
	@Resource
	protected SelfHealingService selfHealingService;

	@Test
	public void testRemovalOfNonWritablePartOfAttribute()
	{
		//given
		SavedValuesModel savedValues = modelService.create(SavedValuesModel.class);
		SavedValueEntryModel savedValueEntry = modelService.create(SavedValueEntryModel.class);

		savedValues.setTimestamp(new Date());
		savedValues.setModifiedItemDisplayString("Product[PK1213242]");
		savedValues.setModifiedItemType(typeService.getComposedTypeForClass(ProductModel.class));
		savedValues.setModificationType(SavedValueEntryType.CHANGED);
		savedValues.setUser(userService.getCurrentUser());

		savedValueEntry.setParent(savedValues);
		savedValueEntry.setModifiedAttribute(ProductModel.DESCRIPTION);
		savedValueEntry.setOldValueAttributeDescriptor(
				typeService.getComposedTypeForClass(ProductModel.class).getCatalogVersionAttribute());

		modelService.saveAll(savedValues, savedValueEntry);

		//when
		modelService.remove(savedValues);

		//then
		assertThat(modelService.isRemoved(savedValues)).isTrue();
		assertThat(modelService.isRemoved(savedValueEntry)).isTrue();
	}

	@Test
	public void testGetFilteredPartOfAttributesOfItemModelConverterDoesntFilterProperAttributesForUserModel()
	{
		ItemModelConverter itemModelConverter = createModelConverter("User", UserModel.class);
		PartOfModelRegisterForRemoveInterceptor partOfModelRegisterForRemoveInterceptor = new PartOfModelRegisterForRemoveInterceptor();
		partOfModelRegisterForRemoveInterceptor.setTypeService(typeService);

		Set<String> filteredCollection = partOfModelRegisterForRemoveInterceptor.getFilteredPartOfAttributes(itemModelConverter);
		Set<String> unfilteredCollection = itemModelConverter.getPartOfAttributes(typeService);

		assertThat(unfilteredCollection).isNotEmpty().contains("allDocuments");
		assertThat(unfilteredCollection).containsAll(filteredCollection);
		assertThat(filteredCollection).doesNotContain("allDocuments");
	}

	@Test
	public void testGetFilteredPartOfAttributesOfItemModelConverterDoesntReturnAllDocumentsAWhileGetPartOfAttributesDoesForTitleModel()
	{
		ItemModelConverter itemModelConverter = createModelConverter("Title", TitleModel.class);
		PartOfModelRegisterForRemoveInterceptor partOfModelRegisterForRemoveInterceptor = new PartOfModelRegisterForRemoveInterceptor();
		partOfModelRegisterForRemoveInterceptor.setTypeService(typeService);

		assertThat(partOfModelRegisterForRemoveInterceptor.getFilteredPartOfAttributes(itemModelConverter)).doesNotContain(
				"allDocuments");
		assertThat(itemModelConverter.getPartOfAttributes(typeService)).isNotEmpty();
		assertThat(itemModelConverter.getPartOfAttributes(typeService)).contains("allDocuments");
	}

	private ItemModelConverter createModelConverter(final String type, Class modelClass)
	{
		final ConfigIntf realCfg = Registry.getCurrentTenant().getConfig();
		final ConfigIntf testCfg = new FastHashMapConfig(Collections.EMPTY_MAP);
		final ConfigIntf fallbackCfg = new FallbackConfig(testCfg, realCfg);

		// switch off preFetching
		testCfg.setParameter(ServicelayerConstants.PARAM_PREFETCH, ServicelayerConstants.VALUE_PREFETCH_NONE);

		final ItemModelConverter conv = new ItemModelConverter(modelService, i18nService, commonI18NService, type,
				modelClass, null, sourceTransformer, selfHealingService)
		{
			@Override
			protected ConfigIntf getConfig()
			{
				return fallbackCfg;
			}
		};
		conv.init(((DefaultModelService) modelService).getConverterRegistry());

		return conv;
	}
}
