/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.property;

import static de.hybris.platform.persistence.property.HJMPCachePopulator.POPULATOR_ENABLED;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.Registry;
import de.hybris.platform.persistence.ItemHome;
import de.hybris.platform.persistence.c2l.LocalizableItemRemote;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.AbstractItemModel;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.jeeapi.YFinderException;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.After;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

@IntegrationTest
public class HJMPLpCachePopulatorTest extends ServicelayerBaseTest
{
	private final HJMPLpCachePopulator hjmpLpCachePopulator = new HJMPLpCachePopulator();
	private final HJMPCachePopulator hjmpCachePopulator = new HJMPCachePopulator();

	private final PropertyConfigSwitcher property = new PropertyConfigSwitcher(POPULATOR_ENABLED);

	@Before
	public void setUp()
	{
		property.switchToValue("true");
		Assume.assumeFalse(hjmpCachePopulator.preconditionsAreNotFulfilled());
	}

	@After
	public void tearDown()
	{
		property.switchBackToDefault();
	}

	@Test
	public void checkIfTypeSystemLpBatchLoadingIsEquivalentToLoadingItemByItem() throws YFinderException
	{
		for (final HJMPLpCachePopulator.TypeSystemTypeCode typeSystemTypeCode : de.hybris.platform.persistence.property.HJMPLpCachePopulator.TypeSystemTypeCode.values())
		{
			final int typeCode = typeSystemTypeCode.getTypeCode();
			final Multimap<PK, EJBPropertyRowCache> multimapItemByItem = loadToMultimapLpRowCacheItemByItem(
					typeCode);
			final Multimap<PK, EJBPropertyRowCache> multimapBatchLoad = loadToMultimapLpRowCacheInBatchWay(
					typeCode);

			assertThat(multimapBatchLoad).isEqualTo(multimapItemByItem);
		}
	}

	@Test
	public void checkIfGetSupportedLanguagesViaJaloIsEquivalentToPreviousWay()
	{
		final List<PK> languagesCurrentWay = hjmpLpCachePopulator.getSupportedLanguagesPkList();
		final List<PK> languagesPreviousWay = getSupportedLanguagesPkListServiceLayer();

		assertThat(languagesPreviousWay.size()).isEqualTo(languagesCurrentWay.size());
		assertThat(languagesPreviousWay.toArray()).containsExactlyInAnyOrder(languagesPreviousWay.toArray());
	}

	/**
	 * (batch) Way of loading localized properties during server startup [hjmp.bulk.load.enabled=true]
	 */
	private Multimap<PK, EJBPropertyRowCache> loadToMultimapLpRowCacheInBatchWay(final int typeCode)
	{
		return hjmpLpCachePopulator.cacheLpForTypeCode(typeCode);
	}

	/**
	 * Way of loading localized properties during (by backoffice [ref]) server startup  (item by item) [hjmp.bulk.load.enabled=false]
	 * [ref] com.hybris.backoffice.config.WarmUpTypeFacadeCaches.warmUpCaches
	 */
	private Multimap<PK, EJBPropertyRowCache> loadToMultimapLpRowCacheItemByItem(final int typeCode)
			throws YFinderException
	{
		final ItemHome itemHome = hjmpLpCachePopulator.getItemHome(typeCode);
		final Collection<LocalizableItemRemote> items = itemHome.findAll();
		final List<PK> languages = hjmpLpCachePopulator.getSupportedLanguagesPkList();

		final Multimap<PK, EJBPropertyRowCache> multimap = ArrayListMultimap.create();

		for (final LocalizableItemRemote item : items)
		{
			final PK itemPK = item.getPK();
			final PK typeKey = item.getTypeKey();
			final TypeInfoMap typeInfoMap = item.getTypeInfoMap();
			final long propertyTimestamp = item.getPropertyTimestamp();

			for (final PK lang : languages)
			{
				final EJBPropertyRowCache properties = PropertyJDBC.getProperties(typeInfoMap, itemPK, typeKey, lang,
						propertyTimestamp);
				multimap.put(itemPK, properties);
			}
		}
		return multimap;
	}

	// previous way of obtaining languages
	private List<PK> getSupportedLanguagesPkListServiceLayer()
	{
		final CommonI18NService commonI18NService = Registry.getApplicationContext()
		                                                    .getBean("commonI18NService", CommonI18NService.class);
		return commonI18NService.getAllLanguages().stream().map(AbstractItemModel::getPk).collect(Collectors.toList());
	}
}