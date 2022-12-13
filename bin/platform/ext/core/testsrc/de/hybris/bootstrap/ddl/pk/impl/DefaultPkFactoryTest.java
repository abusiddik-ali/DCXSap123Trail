/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.bootstrap.ddl.pk.impl;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;
import static org.mockito.BDDMockito.given;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.bootstrap.ddl.dbtypesystem.NumberSeries;
import de.hybris.bootstrap.ddl.pk.PkFactory;
import de.hybris.bootstrap.typesystem.YAtomicType;
import de.hybris.bootstrap.typesystem.YAttributeDescriptor;
import de.hybris.bootstrap.typesystem.YCollectionType;
import de.hybris.bootstrap.typesystem.YComposedType;
import de.hybris.bootstrap.typesystem.YDeployment;
import de.hybris.bootstrap.typesystem.YEnumType;
import de.hybris.bootstrap.typesystem.YEnumValue;
import de.hybris.bootstrap.typesystem.YMapType;
import de.hybris.platform.core.PK;

import java.util.Map;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.runners.MockitoJUnitRunner;

import com.google.common.collect.Lists;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class DefaultPkFactoryTest
{
	private static final int TEST_CLUSER_ID = 0;

	@Mock
	private NumberSeries numberSeries1, numberSeries2, numberSeries3;
	@Mock
	private YComposedType yComposedType, metaType;
	@Mock
	private YAttributeDescriptor yAttributeDescriptor;
	@Mock
	private YEnumValue yEnumValue;
	@Mock
	private YEnumType yEnumType;
	@Mock
	private YAtomicType yAtomicType;
	@Mock
	private YMapType yMapType;
	@Mock
	private YCollectionType yCollectionType;
	@Mock
	private YDeployment yDeployment;

	@Before
	public void setUp()
	{
		MockitoAnnotations.initMocks(this);
		given(numberSeries1.getSeriesKey()).willReturn("pk_1");
		given(numberSeries1.getValue()).willReturn(getCurrentCounter(10));

		given(numberSeries2.getSeriesKey()).willReturn("pk_2");
		given(numberSeries2.getValue()).willReturn(getCurrentCounter(15));

		given(numberSeries3.getSeriesKey()).willReturn("pk_3");
		given(numberSeries3.getValue()).willReturn(getCurrentCounter(20));
	}

	@Test
	public void shouldReturnMapOfCurrentNumberSeries()
	{
		// given
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);

		// when
		final Map<String, Long> currentNumberSeries = pkFactory.getCurrentNumberSeries();

		// then
		assertThat(currentNumberSeries).hasSize(3);
		assertThat(currentNumberSeries.get("pk_1")).isEqualTo(10);
		assertThat(currentNumberSeries.get("pk_2")).isEqualTo(15);
		assertThat(currentNumberSeries.get("pk_3")).isEqualTo(20);
	}

	@Test
	public void mapOfCurrentNumberSeriesShouldBeUnmodifable()
	{
		// given
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);

		try
		{
			// when
			final Map<String, Long> currentNumberSeries = pkFactory.getCurrentNumberSeries();
			currentNumberSeries.put("pk_1", 200L);
			fail("Should throw UnsupportedOperationException");
		}
		catch (final UnsupportedOperationException e)
		{
			// then OK
		}
	}

	@Test
	public void shouldGeneratePksForNewTypesBasedOnNewlyInitializedCounter()
	{
		// given
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);

		// when
		final PK newPK = pkFactory.createNewPK(getTypeCode(5));

		// then
		assertThat(newPK).isNotNull();
		PkAssert.assertThat(newPK).hasTypeAndCounter(getTypeCode(5), getCurrentCounter(1));
	}

	@Test
	public void shouldGeneratePksBasedOnCurrentCounterForExistingTypeCode()
	{
		// given
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);

		// when
		final PK newPK = pkFactory.createNewPK(getTypeCode(1));
		final PK newPK2 = pkFactory.createNewPK(getTypeCode(1));

		// then
		assertThat(newPK).isNotNull();
		assertThat(newPK2).isNotNull();
		PkAssert.assertThat(newPK).hasTypeAndCounter(getTypeCode(1), getCurrentCounter(10));
		PkAssert.assertThat(newPK2).hasTypeAndCounter(getTypeCode(1), getCurrentCounter(11));
	}

	@Test
	public void shouldThrowNullPointerExceptionWhenDeploymentForComposedTypeIsNull()
	{
		// given
		given(yComposedType.getDeployment()).willReturn(null);
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);

		try
		{
			// when
			pkFactory.createNewPK(yComposedType);
			fail("Should throw NullPointerException");
		}
		catch (final NullPointerException e)
		{
			// then OK
		}
	}

	@Test
	public void shouldGeneratePkForComposedType()
	{
		// given
		given(yComposedType.getDeployment()).willReturn(yDeployment);
		given(yDeployment.getItemTypeCode()).willReturn(getTypeCode(1));
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);

		// when
		final PK newPK = pkFactory.createNewPK(yComposedType);

		// then
		assertThat(newPK).isNotNull();
		PkAssert.assertThat(newPK).hasTypeAndCounter(getTypeCode(1), getCurrentCounter(10));
	}

	@Test
	public void shouldCreatePkForGivenComposedType()
	{
		// given
		given(yComposedType.getMetaType()).willReturn(metaType);
		given(yComposedType.getCode()).willReturn("TestItem");
		given(metaType.getDeployment()).willReturn(yDeployment);
		given(yDeployment.getItemTypeCode()).willReturn(getTypeCode(4));
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);

		// when
		final PK newPk = pkFactory.getOrCreatePK(yComposedType);

		// then
		assertThat(newPk).isNotNull();
		PkAssert.assertThat(newPk).hasTypeAndCounter(getTypeCode(4), getCurrentCounter(1));
	}

	@Test
	public void shouldReturnExistingPkForGivenComposedTypeWhenItWasAlreadyCreated()
	{
		// given
		given(yComposedType.getMetaType()).willReturn(metaType);
		given(yComposedType.getCode()).willReturn("TestItem");
		given(metaType.getDeployment()).willReturn(yDeployment);
		given(yDeployment.getItemTypeCode()).willReturn(getTypeCode(4));
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);
		final PK newPk = pkFactory.getOrCreatePK(yComposedType);

		// when
		final PK pk = pkFactory.getOrCreatePK(yComposedType);

		// then
		assertThat(pk).isNotNull();
		PkAssert.assertThat(newPk).hasTypeAndCounter(getTypeCode(4), getCurrentCounter(1));
		assertThat(pk).isEqualTo(newPk);
	}

	@Test
	public void shouldCreatePkForGivenEnumValue()
	{
		// given
		given(yEnumValue.getEnumTypeCode()).willReturn("Foo");
		given(yEnumValue.getCode()).willReturn("Bar");
		given(yEnumValue.getEnumType()).willReturn(yEnumType);
		given(yEnumType.getDeployment()).willReturn(yDeployment);
		given(yDeployment.getItemTypeCode()).willReturn(getTypeCode(4));
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);

		// when
		final PK newPk = pkFactory.getOrCreatePK(yEnumValue);

		// then
		assertThat(newPk).isNotNull();
		PkAssert.assertThat(newPk).hasTypeAndCounter(getTypeCode(4), getCurrentCounter(1));
	}

	@Test
	public void shouldReturnExistingPkForGivenEnumValueWhenItWasAlreadyCreated()
	{
		// given
		given(yEnumValue.getEnumTypeCode()).willReturn("Foo");
		given(yEnumValue.getCode()).willReturn("Bar");
		given(yEnumValue.getEnumType()).willReturn(yEnumType);
		given(yEnumType.getDeployment()).willReturn(yDeployment);
		given(yDeployment.getItemTypeCode()).willReturn(getTypeCode(4));
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);
		final PK newPk = pkFactory.getOrCreatePK(yEnumValue);

		// when
		final PK pk = pkFactory.getOrCreatePK(yEnumValue);

		// then
		assertThat(pk).isNotNull();
		PkAssert.assertThat(pk).hasTypeAndCounter(getTypeCode(4), getCurrentCounter(1));
		assertThat(pk).isEqualTo(newPk);
	}

	@Test
	public void shouldCreatePkForGivenAtomicType()
	{
		// given
		given(yAtomicType.getCode()).willReturn("TestAtomicType");
		given(yAtomicType.getMetaType()).willReturn(metaType);
		given(metaType.getDeployment()).willReturn(yDeployment);
		given(yDeployment.getItemTypeCode()).willReturn(getTypeCode(4));
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);

		// when
		final PK newPk = pkFactory.getOrCreatePK(yAtomicType);

		// then
		assertThat(newPk).isNotNull();
		PkAssert.assertThat(newPk).hasTypeAndCounter(getTypeCode(4), getCurrentCounter(1));
	}

	@Test
	public void shouldReturnExistingPkForGivenAtomicTypeWhenItWasAlreadyCreated()
	{
		// given
		given(yAtomicType.getCode()).willReturn("TestAtomicType");
		given(yAtomicType.getMetaType()).willReturn(metaType);
		given(metaType.getDeployment()).willReturn(yDeployment);
		given(yDeployment.getItemTypeCode()).willReturn(getTypeCode(4));
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);
		final PK newPk = pkFactory.getOrCreatePK(yAtomicType);

		// when
		final PK pk = pkFactory.getOrCreatePK(yAtomicType);

		// then
		assertThat(pk).isNotNull();
		PkAssert.assertThat(pk).hasTypeAndCounter(getTypeCode(4), getCurrentCounter(1));
		assertThat(pk).isEqualTo(newPk);
	}

	@Test
	public void shouldCreatePkForGivenMapType()
	{
		// given
		given(yMapType.getCode()).willReturn("TestMapType");
		given(yMapType.getMetaType()).willReturn(metaType);
		given(metaType.getDeployment()).willReturn(yDeployment);
		given(yDeployment.getItemTypeCode()).willReturn(getTypeCode(4));
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);

		// when
		final PK newPk = pkFactory.getOrCreatePK(yMapType);

		// then
		assertThat(newPk).isNotNull();
		PkAssert.assertThat(newPk).hasTypeAndCounter(getTypeCode(4), getCurrentCounter(1));
	}

	@Test
	public void shouldReturnExistingPkForGivenMapTypeWhenItWasAlreadyCreated()
	{
		// given
		given(yMapType.getCode()).willReturn("TestMapType");
		given(yMapType.getMetaType()).willReturn(metaType);
		given(metaType.getDeployment()).willReturn(yDeployment);
		given(yDeployment.getItemTypeCode()).willReturn(getTypeCode(4));
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);
		final PK newPk = pkFactory.getOrCreatePK(yMapType);

		// when
		final PK pk = pkFactory.getOrCreatePK(yMapType);

		// then
		assertThat(pk).isNotNull();
		PkAssert.assertThat(pk).hasTypeAndCounter(getTypeCode(4), getCurrentCounter(1));
		assertThat(pk).isEqualTo(newPk);
	}

	@Test
	public void shouldCreatePkForGivenCollectionType()
	{
		// given
		given(yCollectionType.getCode()).willReturn("TestCollectionType");
		given(yCollectionType.getMetaType()).willReturn(metaType);
		given(metaType.getDeployment()).willReturn(yDeployment);
		given(yDeployment.getItemTypeCode()).willReturn(getTypeCode(4));
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);

		// when
		final PK newPk = pkFactory.getOrCreatePK(yCollectionType);

		// then
		assertThat(newPk).isNotNull();
		PkAssert.assertThat(newPk).hasTypeAndCounter(getTypeCode(4), getCurrentCounter(1));
	}

	@Test
	public void shouldReturnExistingPkForGivenCollectionTypeWhenItWasAlreadyCreated()
	{
		// given
		given(yCollectionType.getCode()).willReturn("TestCollectionType");
		given(yCollectionType.getMetaType()).willReturn(metaType);
		given(metaType.getDeployment()).willReturn(yDeployment);
		given(yDeployment.getItemTypeCode()).willReturn(getTypeCode(4));
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);
		final PK newPk = pkFactory.getOrCreatePK(yCollectionType);

		// when
		final PK pk = pkFactory.getOrCreatePK(yCollectionType);

		// then
		assertThat(pk).isNotNull();
		PkAssert.assertThat(pk).hasTypeAndCounter(getTypeCode(4), getCurrentCounter(1));
		assertThat(pk).isEqualTo(newPk);
	}

	@Test
	public void shouldCreatePkForGivenComposedTypeAndAttributeDescriptor()
	{
		// given
		given(yComposedType.getCode()).willReturn("TestItem");
		given(yAttributeDescriptor.getQualifier()).willReturn("fooBar");
		given(yAttributeDescriptor.getMetaType()).willReturn(metaType);
		given(metaType.getDeployment()).willReturn(yDeployment);
		given(yDeployment.getItemTypeCode()).willReturn(getTypeCode(4));
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);

		// when
		final PK newPk = pkFactory.getOrCreatePK(yComposedType, yAttributeDescriptor);

		// then
		assertThat(newPk).isNotNull();
		PkAssert.assertThat(newPk).hasTypeAndCounter(getTypeCode(4), getCurrentCounter(1));
	}

	@Test
	public void shouldReturnExistingPkForGivenComposedTypeAndAttributeDescriptorWhenItWasAlreadyCreated()
	{
		// given
		given(yComposedType.getCode()).willReturn("TestItem");
		given(yAttributeDescriptor.getQualifier()).willReturn("fooBar");
		given(yAttributeDescriptor.getMetaType()).willReturn(metaType);
		given(metaType.getDeployment()).willReturn(yDeployment);
		given(yDeployment.getItemTypeCode()).willReturn(getTypeCode(4));
		final PkFactory pkFactory = new DefaultPkFactory(Lists.newArrayList(numberSeries1, numberSeries2, numberSeries3),
				TEST_CLUSER_ID);
		final PK newPk = pkFactory.getOrCreatePK(yComposedType, yAttributeDescriptor);

		// when
		final PK pk = pkFactory.getOrCreatePK(yComposedType, yAttributeDescriptor);

		// then
		assertThat(pk).isNotNull();
		PkAssert.assertThat(pk).hasTypeAndCounter(getTypeCode(4), getCurrentCounter(1));
		assertThat(pk).isEqualTo(newPk);
	}

	private int getTypeCode(final int typeCode)
	{
		return typeCode;
	}

	private long getCurrentCounter(final long currentCounter)
	{
		return currentCounter;
	}

	private static class PkAssert extends AbstractAssert<PkAssert, PK>
	{
		private PkAssert(final PK actual)
		{
			super(actual, PkAssert.class);
		}

		public static PkAssert assertThat(final PK actual)
		{
			return new PkAssert(actual);
		}

		public PkAssert hasTypeAndCounter(final int typeCode, final long counter)
		{
			Assertions.assertThat(actual.getTypeCode())
			          .overridingErrorMessage(
					          "Typecode mismatch [expected: " + typeCode + ", actual: " + actual.getTypeCode() + "]")
			          .isEqualTo(typeCode);
			Assertions.assertThat(actual.getCounter())
			          .overridingErrorMessage("Counter mismatch [expected: " + counter + ", actual: " + actual.getCounter() + "]")
			          .isEqualTo(counter);

			return this;
		}
	}

}
