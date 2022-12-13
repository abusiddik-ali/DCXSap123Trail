/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.internal.model;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.type.AttributeDescriptorModel;
import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.type.TypeService;

import java.util.Collection;

import javax.annotation.Resource;

import org.junit.Test;

@IntegrationTest
public class ComposedTypeIntegrationTest extends ServicelayerTransactionalBaseTest
{
	@Resource
	private ModelService modelService;

	@Resource
	private TypeService typeService;


	@Test
	public void composedTypeAttributeShouldHaveTheSameHandlerAsAttributeFromSuperclass()
	{
		//given
		final ComposedTypeModel product = (ComposedTypeModel) typeService.getTypeForCode("Product");
		final AttributeDescriptorModel europe1DiscountsProduct = product.getDeclaredattributedescriptors()
		                                                                .stream()
		                                                                .filter(attr -> attr.getQualifier()
		                                                                                    .equals(ProductModel.EUROPE1DISCOUNTS))
		                                                                .findFirst().get();
		//when
		final ComposedTypeModel myType = modelService.create(ComposedTypeModel.class);
		myType.setSuperType(product);
		myType.setCode("SomeSuperTypeExtendingProduct");
		myType.setGenerate(product.getGenerate());
		myType.setSingleton(product.getSingleton());
		myType.setCatalogItemType(product.getCatalogItemType());
		modelService.saveAll();

		final AttributeDescriptorModel europe1DiscountsMyType = myType.getInheritedattributedescriptors()
		                                                              .stream()
		                                                              .filter(attr -> attr.getQualifier()
		                                                                                  .equals(ProductModel.EUROPE1DISCOUNTS))
		                                                              .findFirst().get();

		//then
		assertThat(europe1DiscountsMyType.getAttributeHandler()).isEqualTo(europe1DiscountsProduct.getAttributeHandler());
		assertThat(europe1DiscountsMyType.getDescription()).isEqualTo(europe1DiscountsProduct.getDescription());
		assertThat(europe1DiscountsMyType.getDatabaseColumn()).isEqualTo(europe1DiscountsProduct.getDatabaseColumn());
		assertThat(europe1DiscountsMyType.getConstraints()).isEqualTo(europe1DiscountsProduct.getConstraints());
		assertThat(europe1DiscountsMyType.getName()).isEqualTo(europe1DiscountsProduct.getName());
		assertThat(europe1DiscountsMyType.getDontCopy()).isEqualTo(europe1DiscountsProduct.getDontCopy());
		assertThat(europe1DiscountsMyType.getEncrypted()).isEqualTo(europe1DiscountsProduct.getEncrypted());
		assertThat(europe1DiscountsMyType.getHiddenForUI()).isEqualTo(europe1DiscountsProduct.getHiddenForUI());
		assertThat(europe1DiscountsMyType.getInitial()).isEqualTo(europe1DiscountsProduct.getInitial());
		assertThat(europe1DiscountsMyType.getLocalized()).isEqualTo(europe1DiscountsProduct.getLocalized());
		assertThat(europe1DiscountsMyType.getOptional()).isEqualTo(europe1DiscountsProduct.getOptional());
		assertThat(europe1DiscountsMyType.getPartOf()).isEqualTo(europe1DiscountsProduct.getPartOf());
		assertThat(europe1DiscountsMyType.getPersistenceClass()).isEqualTo(europe1DiscountsProduct.getPersistenceClass());
		assertThat(europe1DiscountsMyType.getPrimitive()).isEqualTo(europe1DiscountsProduct.getPrimitive());
		assertThat(europe1DiscountsMyType.getPrivate()).isEqualTo(europe1DiscountsProduct.getPrivate());
		assertThat(europe1DiscountsMyType.getProperty()).isEqualTo(europe1DiscountsProduct.getProperty());
		assertThat(europe1DiscountsMyType.getProposedDatabaseColumn()).isEqualTo(
				europe1DiscountsProduct.getProposedDatabaseColumn());
		assertThat(europe1DiscountsMyType.getReadable()).isEqualTo(europe1DiscountsProduct.getReadable());
		assertThat(europe1DiscountsMyType.getReadOnlyForUI()).isEqualTo(europe1DiscountsProduct.getReadOnlyForUI());
		assertThat(europe1DiscountsMyType.getRemovable()).isEqualTo(europe1DiscountsProduct.getRemovable());
		assertThat(europe1DiscountsMyType.getSearch()).isEqualTo(europe1DiscountsProduct.getSearch());
		assertThat(europe1DiscountsMyType.getUnique()).isEqualTo(europe1DiscountsProduct.getUnique());
		assertThat(europe1DiscountsMyType.getWritable()).isEqualTo(europe1DiscountsProduct.getWritable());
	}

	@Test
	public void composedTypeAttributeShouldHaveTheSameHiddenForUIAsAttributeFromSuperclass()
	{
		// given
		final ComposedTypeModel product = (ComposedTypeModel) typeService.getTypeForCode("Product");
		final AttributeDescriptorModel europe1DiscountsProductDescriptor = getEurope1DiscountsProductDescriptor(product);

		// when
		// responsible method: de.hybris.platform.persistence.type.TypeManagerEJB.copyDownParentAttributeDescriptor worth to notice that some attributes are still not being copied
		europe1DiscountsProductDescriptor.setHiddenForUI(true);
		modelService.saveAll();
		final ComposedTypeModel subProduct = createProductRelatedCT(product);
		modelService.saveAll();
		final AttributeDescriptorModel europe1DiscountsSubProductDescriptor = getEurope1DiscountsProductDescriptor(subProduct,
				true);
		// then
		assertThat(europe1DiscountsProductDescriptor.getHiddenForUI()).isEqualTo(true);
		assertThat(europe1DiscountsSubProductDescriptor.getHiddenForUI()).isEqualTo(true);
	}

	@Test
	public void composedTypeAttributeShouldHaveTheSameDontCopyAsAttributeFromSuperclass()
	{
		// given
		final ComposedTypeModel product = (ComposedTypeModel) typeService.getTypeForCode("Product");
		final AttributeDescriptorModel europe1DiscountsProductDescriptor = getEurope1DiscountsProductDescriptor(product);

		// when
		europe1DiscountsProductDescriptor.setDontCopy(true);
		modelService.saveAll();
		final ComposedTypeModel subProduct = createProductRelatedCT(product);
		modelService.saveAll();
		final AttributeDescriptorModel europe1DiscountsSubProductDescriptor = getEurope1DiscountsProductDescriptor(subProduct,
				true);
		// then
		assertThat(europe1DiscountsProductDescriptor.getDontCopy()).isEqualTo(true);
		assertThat(europe1DiscountsSubProductDescriptor.getDontCopy()).isEqualTo(true);
	}

	private ComposedTypeModel createProductRelatedCT(final ComposedTypeModel product)
	{
		final ComposedTypeModel myType = modelService.create(ComposedTypeModel.class);
		myType.setSuperType(product);
		myType.setCode("subProduct");
		myType.setGenerate(product.getGenerate());
		myType.setSingleton(product.getSingleton());
		myType.setCatalogItemType(product.getCatalogItemType());
		return myType;
	}

	private AttributeDescriptorModel getEurope1DiscountsProductDescriptor(final ComposedTypeModel composedTypeModel)
	{
		return getEurope1DiscountsProductDescriptor(composedTypeModel, false);
	}

	private AttributeDescriptorModel getEurope1DiscountsProductDescriptor(final ComposedTypeModel composedTypeModel,
	                                                                      final boolean inherited)
	{
		final Collection<AttributeDescriptorModel> attributes = inherited ? composedTypeModel.getInheritedattributedescriptors() : composedTypeModel
				.getDeclaredattributedescriptors();

		return attributes
				.stream()
				.filter(attr -> attr.getQualifier()
				                    .equals(ProductModel.EUROPE1DISCOUNTS))
				.findFirst().get();
	}
}

