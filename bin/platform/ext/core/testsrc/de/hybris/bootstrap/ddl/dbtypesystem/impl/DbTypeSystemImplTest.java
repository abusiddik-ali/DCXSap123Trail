/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.bootstrap.ddl.dbtypesystem.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.bootstrap.ddl.dbtypesystem.Attribute;
import de.hybris.bootstrap.ddl.dbtypesystem.DbTypeSystem;
import de.hybris.bootstrap.ddl.dbtypesystem.Deployment;
import de.hybris.bootstrap.ddl.dbtypesystem.Type;

import java.util.Collection;

import org.assertj.core.api.AbstractObjectAssert;
import org.assertj.core.api.Condition;
import org.assertj.core.api.ObjectAssert;
import org.junit.Before;
import org.junit.Test;

import com.google.common.base.Function;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableSet;


@UnitTest
public class DbTypeSystemImplTest
{

	private DbTypeSystem typeSystem;

	@Before
	public void prepare()
	{

		typeSystem = new DbTypeSystemImpl(new TestTypeSystemRows());
		((DbTypeSystemImpl) typeSystem).initialize();
	}

	@Test
	public void shouldBeAbleToFindExistingTypeByCode()
	{
		//when
		final Type type = typeSystem.findTypeByCode("Product");

		//then
		assertThatType(type).hasCode("Product");
	}

	@Test
	public void shouldBeAbleToFindExistingTypeByPk()
	{

		//when
		final Type type = typeSystem.findTypeByPK(1900626L);

		//then
		assertThatType(type).hasCode("Product");
	}

	@Test
	public void shouldNotBeAbleToFindNotExistingTypeByName()
	{
		//when
		final Type type = typeSystem.findTypeByCode("NotExistingType");

		//then
		assertThatType(type).isNull();
	}

	@Test
	public void shouldNotBeAbleToFindNotExistingTypeByPk()
	{
		//when
		final Type type = typeSystem.findTypeByPK(1234554321L);

		//then
		assertThatType(type).isNull();
	}

	@Test
	public void shouldBeAbleToFindRootType()
	{
		//when
		final Type rootType = typeSystem.findTypeByCode("Item");

		//then
		assertThatType(rootType).isRootType();
	}

	@Test
	public void shouldBeAbleToFindNotRootType()
	{
		//when
		final Type notRootType = typeSystem.findTypeByCode("Product");

		//then
		assertThatType(notRootType).isNotRootType();
	}

	@Test
	public void shouldBeAbleToGoUpIntoInheritanceHierarchy()
	{
		//when
		final Type type = typeSystem.findTypeByCode("Product");

		//then
		assertThatType(type). //
				                      isDirectSubtypeOf("GenericItem"). //
						                                                        isDirectSubtypeOf("LocalizableItem"). //
								                                                                                              isDirectSubtypeOf(
				"ExtensibleItem"). //
						                   isDirectSubtypeOf("Item"). //
								                                              isRootType();
	}

	@Test
	public void shouldBeAbleToFindDeploymentByFullName()
	{
		//given
		final String packageName = "de.hybris.platform.persistence";
		final String deploymentName = "core_Product";
		final String fullName = packageName + "." + deploymentName;

		//when
		final Deployment deployment = typeSystem.findDeploymentByFullName(fullName);

		//then
		assertThatDeployment(deployment). //
				                                  exists(). //
						                                            hasFullName(fullName);
	}

	@Test
	public void shouldBeAbleToFindDeploymentForType()
	{
		//given
		final Type type = typeSystem.findTypeByCode("Product");
		final String packageName = "de.hybris.platform.persistence";
		final String deploymentName = "core_Product";
		final String fullName = packageName + "." + deploymentName;

		//when
		final Deployment deployment = type.getDeployment();

		//then
		assertThatDeployment(deployment). //
				                                  exists(). //
						                                            hasFullName(type.getItemJndiName()). //
								                                                                                 hasFullName(
				fullName);
	}

	@Test
	public void shouldNotBeAbleToFindNotExistingDeployment()
	{
		//when
		final Deployment deployment = typeSystem.findDeploymentByFullName("not.existing.Deployment");

		//then
		assertThatDeployment(deployment).notExist();
	}

	@Test
	public void shouldFindAttributeByPk()
	{
		//when
		final Attribute attribute = typeSystem.findAttributeByPk(296910935L);

		//then
		assertThatAttribute(attribute). //
				                                hasQualifier("catalog"). //
						                                                         isEnclosedInType("Product");
	}

	@Test
	public void shouldNotFindAttributeForNotExistingPk()
	{

		//when
		final Attribute attribute = typeSystem.findAttributeByPk(1234554321L);

		//then
		assertThatAttribute(attribute).isNull();
	}

	@Test
	public void shouldFindAttributesForType()
	{
		//given
		final Type type = typeSystem.findTypeByCode("Product");

		//when
		final Collection<Attribute> attributes = type.getAttributes();

		//then
		assertThat(attributes).isNotEmpty().has(new Condition<>(){
			@Override
			public boolean matches(final Iterable<? extends Attribute> attributesToTest)
			{
				for (final Object a : attributesToTest)
				{
					assertThatType(((Attribute) a).getEnclosingType()).hasCode("Product");
				}
				return true;
			}
		});
	}

	@Test
	public void typeShouldContainAttributesFromSuperType()
	{
		//given
		final Type type = typeSystem.findTypeByCode("Product");
		final Collection<Attribute> attributes = type.getAttributes();
		final Collection<Attribute> superAttributes = type.getSuperType().getAttributes();
		final Function<Attribute, String> qualifierExtractor = new Function<Attribute, String>()
		{
			@Override
			public String apply(final Attribute attribute)
			{
				return attribute.getQualifierLowerCaseInternal();
			}
		};

		//when
		final ImmutableSet<String> qualifiers = FluentIterable.from(attributes).transform(qualifierExtractor).toSet();
		final ImmutableSet<String> superQualifiers = FluentIterable.from(superAttributes).transform(qualifierExtractor).toSet();

		//then
		assertThat(qualifiers.containsAll(superQualifiers)).isTrue();
	}


	private static TypeAssert assertThatType(final Type type)
	{
		return new TypeAssert(type);
	}

	private static DeploymentAssert assertThatDeployment(final Deployment deployment)
	{
		return new DeploymentAssert(deployment);
	}

	private static AttributeAssert assertThatAttribute(final Attribute attribute)
	{
		return new AttributeAssert(attribute);
	}

	private static class TypeAssert extends AbstractObjectAssert<TypeAssert, Type>
	{
		private final Type type;

		protected TypeAssert(final Type type)
		{
			super(type, TypeAssert.class);
			this.type = type;
		}

		public TypeAssert isDirectSubtypeOf(final String typeCode)
		{
			final String superTypeDoesNotExist = String.format("Supertype of type %s can't be found.",
					type.getInternalCodeLowerCase());
			isNotNull();

			assertThat(type.getSuperType()).overridingErrorMessage(superTypeDoesNotExist).isNotNull();
			final String superTypeDoesNotMatch = String.format("Expecting type %s but type %s has been found.", typeCode, type
					.getSuperType().getInternalCodeLowerCase());
			assertThat(type.getSuperType().getInternalCodeLowerCase()).overridingErrorMessage(superTypeDoesNotMatch)
			                                                          .isEqualToIgnoringCase(typeCode);
			return new TypeAssert(type.getSuperType());
		}

		public TypeAssert isNotRootType()
		{
			isNotNull();
			assertThat(type.getSuperType()).overridingErrorMessage("Not root type should have super type.").isNotNull();
			return this;
		}

		public TypeAssert isRootType()
		{
			isNotNull();
			assertThat(type.getSuperType()).overridingErrorMessage("Root type shouldn't have super type.").isNull();
			return this;
		}

		public TypeAssert hasCode(final String code)
		{
			isNotNull();
			assertThat(type.getInternalCodeLowerCase()).isEqualToIgnoringCase(code);
			return this;
		}
	}

	private static class DeploymentAssert extends AbstractObjectAssert<DeploymentAssert, Deployment>
	{
		private final Deployment deployment;

		public DeploymentAssert(final Deployment deployment)
		{
			super(deployment, DeploymentAssert.class);
			this.deployment = deployment;
		}

		public DeploymentAssert notExist()
		{
			isNull();
			return this;
		}

		public DeploymentAssert hasFullName(final String fullName)
		{
			assertThat(deployment.getFullName()).isEqualTo(fullName);
			return this;
		}

		public DeploymentAssert exists()
		{
			overridingErrorMessage("Deployment doesn't exists").isNotNull();
			return this;
		}

	}

	private static class AttributeAssert extends ObjectAssert
	{
		private final Attribute attribute;

		public AttributeAssert(final Attribute attribute)
		{
			super(attribute);
			this.attribute = attribute;
		}

		public AttributeAssert isEnclosedInType(final String code)
		{
			isNotNull();
			assertThatType(attribute.getEnclosingType()).hasCode(code);
			return this;
		}

		public AttributeAssert hasQualifier(final String qualifier)
		{
			isNotNull();
			assertThat(attribute.getQualifierLowerCaseInternal()).isEqualToIgnoringCase(qualifier);
			return this;
		}

	}
}
