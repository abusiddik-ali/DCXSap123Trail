/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.type;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.fail;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.type.AttributeDescriptorModel;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.util.Utilities;

import java.util.Collection;
import java.util.UUID;

import javax.annotation.Resource;

import org.apache.log4j.Logger;
import org.junit.Test;



@IntegrationTest
public class DefaultTypeServiceRuntimeAttributesTest extends ServicelayerTransactionalBaseTest
{
	private static final Logger LOG = Logger.getLogger(DefaultTypeServiceRuntimeAttributesTest.class);

	private static final String VALUE = "string-value";

	@Resource
	private ModelService modelService;

	@Resource
	private TypeService typeService;

	@Test
	public void testGetRuntimeAttributesForNullParameter()
	{
		try
		{
			typeService.getRuntimeAttributeDescriptorsForType(null);
			fail("Should throw exception");
		}
		catch (final IllegalArgumentException e)
		{
			// expected
		}
	}

	@Test
	public void testIsRuntimeAttributeForNullParameter()
	{
		try
		{
			typeService.isRuntimeAttribute(null);
			fail("Should throw exception");
		}
		catch (final IllegalArgumentException e)
		{
			// expected
		}
	}

	@Test
	public void testReturnsRuntimeAttributes()
	{
		createTitle();

		final String qualifier = generateQualifier();
		final AttributeDescriptorModel runtimeAttribute = createRuntimeAttribute(TitleModel.class, qualifier);

		final Collection<AttributeDescriptorModel> result = typeService
				.getRuntimeAttributeDescriptorsForType(typeService.getComposedTypeForClass(TitleModel.class));

		assertThat(result).hasSize(1).contains(runtimeAttribute);

		final TitleModel titleModel = prepareTitle(qualifier);

		final String atomicValue1 = modelService.getAttributeValue(titleModel, qualifier);
		assertThat(atomicValue1).isNotNull().isEqualTo(VALUE);
	}

	@Test
	public void testReturnsRuntimeAttributesForSubType()
	{
		createTitle();

		final String qualifier = generateQualifier();

		final AttributeDescriptorModel runtimeAttribute = createRuntimeAttribute(ItemModel.class, qualifier);

		final Collection<AttributeDescriptorModel> results = typeService
				.getRuntimeAttributeDescriptorsForType(typeService.getComposedTypeForClass(TitleModel.class));

		assertThat(results).hasSize(1);

		final AttributeDescriptorModel result = results.iterator().next();
		assertThat(result.getQualifier()).isEqualTo(runtimeAttribute.getQualifier());
		assertThat(result.getAttributeType()).isEqualTo(runtimeAttribute.getAttributeType());
		assertThat(result.getReadable()).isTrue();

		detachAndInvalidateModel(result);

		final TitleModel titleModel = prepareTitle(qualifier);

		final String atomicValue1 = titleModel.getProperty(qualifier);
		assertThat(atomicValue1).isNotNull().isEqualTo(VALUE);

		final String atomicValue2 = modelService.getAttributeValue(titleModel, qualifier);
		assertThat(atomicValue2).isNotNull().isEqualTo(VALUE);
	}

	private TitleModel prepareTitle(final String qualifier)
	{
		final TitleModel title = createTitle();

		title.setProperty(qualifier, VALUE);
		modelService.save(title);
		detachAndInvalidateModel(title);

		return title;
	}

	private TitleModel createTitle()
	{
		final TitleModel title = modelService.create(TitleModel.class);
		title.setCode("TITLE_" + UUID.randomUUID().toString());
		modelService.save(title);
		return title;
	}

	private void detachAndInvalidateModel(final ItemModel modelType)
	{
		Utilities.invalidateCache(modelType.getPk());
		modelService.detach(modelType);
	}

	@Test
	public void testIsRuntimeAttribute()
	{
		final String qualifier = generateQualifier();
		final AttributeDescriptorModel runtimeAttribute = createRuntimeAttribute(TitleModel.class, qualifier);

		assertThat(typeService.isRuntimeAttribute(runtimeAttribute)).isTrue();
	}

	@Test
	public void shouldNotReturnRuntimeAttributeForRootType()
	{
		final String qualifier = generateQualifier();
		final AttributeDescriptorModel runtimeAttribute = createRuntimeAttribute(TitleModel.class, qualifier);
		final Collection<AttributeDescriptorModel> results = typeService
				.getRuntimeAttributeDescriptorsForType(typeService.getComposedTypeForClass(ItemModel.class));
		assertThat(results).isEmpty();
	}

	private String generateQualifier()
	{
		final String qualifier = "runtime-" + UUID.randomUUID();
		return qualifier;
	}

	private AttributeDescriptorModel createRuntimeAttribute(final Class clazz, final String qualifier)
	{
		final AttributeDescriptorModel runtimeAttribute = modelService.create(AttributeDescriptorModel.class);
		runtimeAttribute.setAttributeType(typeService.getAtomicTypeForJavaClass(String.class));
		runtimeAttribute.setEnclosingType(typeService.getComposedTypeForClass(clazz));
		runtimeAttribute.setGenerate(false);
		runtimeAttribute.setPartOf(false);
		runtimeAttribute.setQualifier(qualifier);
		modelService.saveAll();
		modelService.detachAll();

		detachAndInvalidateModel(runtimeAttribute);

		LOG.warn("created attribute");

		return runtimeAttribute;
	}

	@Test
	public void testIsNotRuntimeAttribute()
	{
		final AttributeDescriptorModel attribute = typeService
				.getAttributeDescriptor(typeService.getComposedTypeForClass(TitleModel.class), "code");

		assertThat(typeService.isRuntimeAttribute(attribute)).isFalse();
	}
}
