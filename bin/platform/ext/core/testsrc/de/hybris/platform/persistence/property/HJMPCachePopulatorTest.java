/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.property;

import static de.hybris.platform.persistence.property.HJMPCachePopulator.POPULATOR_ENABLED;

import static org.assertj.core.api.AssertionsForInterfaceTypes.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.jalo.ConsistencyCheckException;
import de.hybris.platform.jalo.product.Product;
import de.hybris.platform.jalo.type.AttributeDescriptor;
import de.hybris.platform.jalo.type.ComposedType;
import de.hybris.platform.jalo.type.JaloDuplicateCodeException;
import de.hybris.platform.jalo.type.JaloDuplicateQualifierException;
import de.hybris.platform.jalo.type.TypeManager;
import de.hybris.platform.persistence.EJBItemNotFoundException;
import de.hybris.platform.persistence.type.AttributeDescriptorHome;
import de.hybris.platform.persistence.type.AttributeDescriptorRemote;
import de.hybris.platform.persistence.type.ComposedTypeHome;
import de.hybris.platform.persistence.type.ComposedTypeRemote;
import de.hybris.platform.persistence.type.TypeManagerEJB;
import de.hybris.platform.regioncache.CacheController;
import de.hybris.platform.regioncache.region.CacheRegion;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.util.jeeapi.YFinderException;
import de.hybris.platform.util.jeeapi.YRemoveException;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;

@IntegrationTest
public class HJMPCachePopulatorTest extends ServicelayerBaseTest
{
	private static final int COMPOSED_TYPE = 2;
	private final HJMPCachePopulator HJMPCachePopulator = new HJMPCachePopulator();
	private final TypeManager typeManager = TypeManager.getInstance();
	private final TypeManagerEJB typeManagerEJB = new TypeManagerEJB();
	private final AttributeDescriptorHome attributeDescriptorHome = HJMPCachePopulator.getAttributeDescriptorHome();
	private final ComposedTypeHome composedTypeHome = HJMPCachePopulator.getComposedTypeHome();
	private final PropertyConfigSwitcher property = new PropertyConfigSwitcher(POPULATOR_ENABLED);

	@Resource
	CacheController cacheController;

	@Before
	public void setUp()
	{
		property.switchToValue("true");
		Assume.assumeFalse(HJMPCachePopulator.preconditionsAreNotFulfilled());
		cleanTypeSystemCacheRegion();
		HJMPCachePopulator.populateCacheHintForHJMP();
	}

	@After
	public void tearDown()
	{
		property.switchBackToDefault();
	}

	@Test
	public void createComposedTypeAndCheckIfIsReturnedCorrectly()
			throws JaloDuplicateCodeException, ConsistencyCheckException,
			YFinderException
	{
		ComposedType testComposedType = null;
		try
		{
			final List<ComposedTypeRemote> composedTypesBefore = (List<ComposedTypeRemote>) HJMPCachePopulator.getAndCacheComposedTypes();
			final int sizeBefore = composedTypesBefore.size();

			final List<ComposedTypeRemote> findByCodeComposedType1 = (List<ComposedTypeRemote>) composedTypeHome.findByCodeExact(
					"myproducttype");


			testComposedType = typeManager.createComposedType(typeManager.getComposedType(Product.class),
					"MyProductType");
			final List<ComposedTypeRemote> composedTypesAfter = (List<ComposedTypeRemote>) HJMPCachePopulator.getAndCacheComposedTypes();
			final int sizeAfter = composedTypesAfter.size();

			final PK findByCodeComposedType2 = ((List<ComposedTypeRemote>) composedTypeHome.findByCodeExact("myproducttype")).get(
					0).getPK();


			assertThat(findByCodeComposedType1).isEqualTo(Collections.emptyList());
			assertThat(testComposedType.getPK()).isEqualTo(findByCodeComposedType2);
			assertThat(sizeAfter).isEqualTo(sizeBefore + 1);
		}
		//cleanUp
		finally
		{
			if (Objects.nonNull(testComposedType))
			{
				testComposedType.remove();
			}
		}
	}

	@Test
	public void createAttributeDescriptorsAndCheckIfAreReturnedCorrectly()
			throws JaloDuplicateCodeException, JaloDuplicateQualifierException, ConsistencyCheckException,
			EJBItemNotFoundException, YRemoveException,
			YFinderException
	{
		AttributeDescriptorRemote test1AttributeDescriptor = null;
		AttributeDescriptorRemote test2AttributeDescriptor = null;
		ComposedType testComposedType = null;
		try
		{
			testComposedType = typeManager.createComposedType(typeManager.getComposedType(Product.class),
					"MyProductType2");

			final int attributesByEnclosingTypeBefore = attributeDescriptorHome.findByEnclosingType(testComposedType.getPK())
			                                                                   .size();

			createAttributeDescriptor(testComposedType, "test2");
			createAttributeDescriptor(testComposedType, "test1");

			final int attributesByEnclosingTypeAfter = attributeDescriptorHome.findByEnclosingType(testComposedType.getPK())
			                                                                  .size();

			test1AttributeDescriptor = getAttributeDescriptor("test1", "MyProductType2");
			test2AttributeDescriptor = getAttributeDescriptor("test2", "MyProductType2");

			assertThat(test1AttributeDescriptor.getQualifier()).isEqualTo("test1");
			assertThat(test2AttributeDescriptor.getQualifier()).isEqualTo("test2");
			assertThat(attributesByEnclosingTypeBefore + 2).isEqualTo(attributesByEnclosingTypeAfter);
		}
		//cleanUp
		finally
		{
			if (Objects.nonNull(test1AttributeDescriptor))
			{
				test1AttributeDescriptor.remove();
			}
			if (Objects.nonNull(test2AttributeDescriptor))
			{
				test2AttributeDescriptor.remove();
			}
			if (Objects.nonNull(testComposedType))
			{
				testComposedType.remove();
			}
		}
	}

	@Test
	public void checkIfPopulateCacheHintForHJMPWorksFine()
	{
		cleanTypeSystemCacheRegion();
		final int sizeBefore = getTypeSystemCacheRegionSize();
		HJMPCachePopulator.populateCacheHintForHJMP();
		final int sizeAfter = getTypeSystemCacheRegionSize();
		//you can't be sure the cache is empty as other process could already load some types
		//just check that cachePopulator loaded all types
		assertThat(sizeAfter).isGreaterThan(sizeBefore);
	}

	private AttributeDescriptorRemote getAttributeDescriptor(final String qualifier, final String code)
			throws EJBItemNotFoundException
	{
		return typeManagerEJB.getAttributeDescriptor(
				(ComposedTypeRemote) typeManagerEJB.getType(code, COMPOSED_TYPE),
				qualifier);
	}

	private void createAttributeDescriptor(final ComposedType composedType, final String qualifier)
			throws JaloDuplicateQualifierException
	{
		composedType.createAttributeDescriptor(qualifier,
				typeManager.getType(String.class.getName()),
				AttributeDescriptor.READ_FLAG | AttributeDescriptor.WRITE_FLAG | AttributeDescriptor.PROPERTY_FLAG);
	}

	private void cleanTypeSystemCacheRegion()
	{
		cacheController.getRegions()
		               .stream()
		               .filter(r -> r.getName()
		                             .equals("typesystemCacheRegion"))
		               .forEach(CacheRegion::clearCache);
	}

	private int getTypeSystemCacheRegionSize()
	{
		return cacheController.getRegions()
		                      .stream()
		                      .filter(r -> r.getName()
		                                    .equals("typesystemCacheRegion"))
		                      .mapToInt(cacheRegion -> cacheRegion.getAllKeys().size()).sum();
	}
}
