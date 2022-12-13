/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.model;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.type.AttributeDescriptorModel;
import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.servicelayer.internal.converter.ConverterRegistry;
import de.hybris.platform.servicelayer.internal.converter.ModelConverter;
import de.hybris.platform.servicelayer.internal.converter.impl.ItemModelConverter;
import de.hybris.platform.servicelayer.type.TypeService;
import de.hybris.platform.util.Utilities;

import java.util.UUID;

public class RefreshModelConverterTestUtils
{

	private final ConverterRegistry converterRegistry;
	private final ModelService modelService;
	private final TypeService typeService;


	public RefreshModelConverterTestUtils(final ConverterRegistry converterRegistry,final ModelService modelService,final TypeService typeService)
	{
		this.converterRegistry = converterRegistry;
		this.modelService = modelService;
		this.typeService = typeService;
	}

	void checkConverterForAttribute(final String sourceType, final String qualifier, final boolean exist)
	{

		final ModelConverter modelConverter = converterRegistry.getModelConverterBySourceType(sourceType);

		if (modelConverter instanceof ItemModelConverter)
		{

			final ItemModelConverter itemModelConverter = (ItemModelConverter) modelConverter;
			final ItemModelConverter.ModelAttributeInfo attribute = itemModelConverter.getInfo(qualifier);
			if (exist)
			{
				assertThat(attribute).isNotNull();
			}
			else
			{
				assertThat(attribute).isNull();
			}
		}

	}

	AttributeDescriptorModel createAttribute(final ComposedTypeModel ctm, final String qualifier)
	{
		final AttributeDescriptorModel runtimeAttribute = modelService.create(AttributeDescriptorModel.class);
		runtimeAttribute.setAttributeType(typeService.getAtomicTypeForJavaClass(Integer.class));
		runtimeAttribute.setEnclosingType(ctm);
		runtimeAttribute.setGenerate(false);
		runtimeAttribute.setPartOf(false);
		runtimeAttribute.setAttributeHandler("dynamicAttributesIntSampleBean");
		runtimeAttribute.setQualifier(qualifier);
		modelService.saveAll();
		detachAndInvalidateModel(runtimeAttribute);
		return runtimeAttribute;
	}

	AttributeDescriptorModel createAttribute(final Class clazz, final String qualifier)
	{

		final ComposedTypeModel ctm = typeService.getComposedTypeForClass(clazz);
		return createAttribute(ctm,qualifier);
	}

	void detachAndInvalidateModel(final ItemModel modelType)
	{
		Utilities.invalidateCache(modelType.getPk());
		modelService.detach(modelType);
	}
	String generateQualifier()
	{
		return "runtime-" + UUID.randomUUID();
	}


}
