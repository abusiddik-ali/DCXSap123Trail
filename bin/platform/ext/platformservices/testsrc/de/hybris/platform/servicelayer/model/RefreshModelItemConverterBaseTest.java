/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.model;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.internal.converter.ConverterRegistry;
import de.hybris.platform.servicelayer.internal.converter.ModelConverter;
import de.hybris.platform.servicelayer.internal.converter.impl.ItemModelConverter;
import de.hybris.platform.servicelayer.type.TypeService;
import de.hybris.platform.test.TestThreadsHolder;

import java.util.Collections;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.stream.IntStream;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;


public class RefreshModelItemConverterBaseTest extends ServicelayerBaseTest
{
	private static final int TERMINATION_WAIT_SECONDS = 60;
	@Resource
	private ModelService modelService;
	@Resource
	private TypeService typeService;
	@Resource
	private ConverterRegistry converterRegistry;
	private RefreshModelConverterTestUtils util;

	@Before
	public void setUp()
	{
		util = new RefreshModelConverterTestUtils(converterRegistry, modelService, typeService);
	}

	@Test
	public void testCheckIfAddedAttributeForItemIsVisibleForSubtypes()
	{

		final String additionalAttribute = util.generateQualifier();

		util.checkConverterForAttribute("item", additionalAttribute, false);
		util.checkConverterForAttribute("title", additionalAttribute, false);
		util.checkConverterForAttribute("product", additionalAttribute, false);
		util.checkConverterForAttribute("user", additionalAttribute, false);

		util.createAttribute(ItemModel.class, additionalAttribute);

		util.checkConverterForAttribute("item", additionalAttribute, true);
		util.checkConverterForAttribute("title", additionalAttribute, true);
		util.checkConverterForAttribute("product", additionalAttribute, true);
		util.checkConverterForAttribute("user", additionalAttribute, true);
	}

	@Test
	public void testCheckIfAddedAttributeForUserIsVisibleForSubtypes()
	{

		final String additionalAttribute = util.generateQualifier();

		util.checkConverterForAttribute("user", additionalAttribute, false);
		util.checkConverterForAttribute("employee", additionalAttribute, false);
		util.checkConverterForAttribute("customer", additionalAttribute, false);

		util.createAttribute(UserModel.class, additionalAttribute);

		util.checkConverterForAttribute("user", additionalAttribute, true);
		util.checkConverterForAttribute("employee", additionalAttribute, true);
		util.checkConverterForAttribute("customer", additionalAttribute, true);
	}

	@Test
	public void testConsistencyDuringAttributeAndItemsCreation()
	{

		final CountDownLatch latch = new CountDownLatch(1);

		final TestThreadsHolder<Runnable> createAttributesThreads = createAttributes(4, latch);
		final TestThreadsHolder<Runnable> createTitlesThreads = createTitles(10, latch);

		latch.countDown();

		assertTrue("not all threads finished in time", createAttributesThreads.waitAndDestroy(TERMINATION_WAIT_SECONDS));
		assertEquals(Collections.emptyMap(), createAttributesThreads.getErrors());
		assertTrue("not all threads finished in time", createTitlesThreads.waitAndDestroy(TERMINATION_WAIT_SECONDS));
		assertEquals(Collections.emptyMap(), createTitlesThreads.getErrors());

	}


	@Test
	public void shouldReturnBaseConverterForNewCreatedComposedTypeWithoutAdditionalAttribute()
	{

		final ComposedTypeModel newComposedTypeModel = new ComposedTypeModel();
		newComposedTypeModel.setCode("newComposedTypeModel");
		newComposedTypeModel.setSuperType(typeService.getComposedTypeForClass(TitleModel.class));
		newComposedTypeModel.setGenerate(false);
		newComposedTypeModel.setSingleton(false);
		newComposedTypeModel.setCatalogItemType(false);
		modelService.save(newComposedTypeModel);

		final String attribQualifier = util.generateQualifier();
		util.createAttribute(newComposedTypeModel, attribQualifier);

		final ModelConverter modelConverter = converterRegistry.getModelConverterBySourceType("newComposedTypeModel");
		if (modelConverter instanceof ItemModelConverter)
		{
			final ItemModelConverter itemModelConverter = (ItemModelConverter) modelConverter;
			assertThat(itemModelConverter.getDefaultType()).isEqualToIgnoringCase("Title");
			assertThat(itemModelConverter.getInfo(attribQualifier)).isNull();
		}
	}

	private TestThreadsHolder<Runnable> createTitles(final int THREADS, final CountDownLatch lock)
	{
		final TestThreadsHolder<Runnable> threads = new TestThreadsHolder<Runnable>(THREADS, true)
		{
			@Override
			public Runnable newRunner(final int threadNumber)
			{
				return () -> {
					try
					{
						lock.await();
					}
					catch (final InterruptedException e)
					{
						throw new RuntimeException(e);
					}
					IntStream.range(0, 10).forEach(i -> createTitle());
				};
			}
		};
		threads.startAll();

		return threads;

	}

	private TestThreadsHolder<Runnable> createAttributes(final int THREADS, final CountDownLatch lock)
	{
		final TestThreadsHolder<Runnable> threads = new TestThreadsHolder<Runnable>(THREADS, true)
		{
			@Override
			public Runnable newRunner(final int threadNumber)
			{
				return () -> {
					try
					{
						lock.await();
					}
					catch (final InterruptedException e)
					{
						throw new RuntimeException(e);
					}
					util.createAttribute(ItemModel.class, util.generateQualifier());
				};
			}
		};
		threads.startAll();
		return threads;
	}

	private TitleModel createTitle()
	{
		final TitleModel title = modelService.create(TitleModel.class);
		title.setCode("TITLE_" + UUID.randomUUID().toString());
		modelService.save(title);
		return title;
	}

}
