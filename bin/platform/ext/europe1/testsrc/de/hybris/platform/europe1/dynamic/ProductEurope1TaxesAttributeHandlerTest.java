/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.europe1.dynamic;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;

import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.order.price.TaxModel;
import de.hybris.platform.europe1.model.TaxRowModel;

import java.util.Arrays;
import java.util.Comparator;
import java.util.function.Function;

import org.assertj.core.api.AbstractIntegerAssert;
import org.junit.runners.Parameterized;


public class ProductEurope1TaxesAttributeHandlerTest extends AbstractProductEurope1AttributeHandlerTest
{

	public ProductEurope1TaxesAttributeHandlerTest(final Function<Void, AbstractIntegerAssert> function)
	{
		super(function);
	}

	@Parameterized.Parameters
	public static Iterable<Object> data()
	{
		return Arrays.asList(//
				compare(null, null).andThen(AbstractIntegerAssert::isZero),

				compareBuilding(left -> left.withUserUid("ab123"), right -> right.withUserUid("AB234"))
						.andThen(AbstractIntegerAssert::isNegative),
				compareBuilding(left -> left.withUserUid("ab234"), right -> right.withUserUid("AB123"))
						.andThen(AbstractIntegerAssert::isPositive),
				compareBuilding(left -> left.withUserUid("theSame"), right -> right.withUserUid("theSame"))
						.andThen(AbstractIntegerAssert::isZero),

				compareBuilding(left -> left.withUgCode("ab123"), right -> right.withUgCode("AB234")).andThen(
						AbstractIntegerAssert::isNegative),
				compareBuilding(left -> left.withUgCode("ab234"), right -> right.withUgCode("AB123")).andThen(
						AbstractIntegerAssert::isPositive),
				compareBuilding(left -> left.withUgCode("theSame"), right -> right.withUgCode("theSame")).andThen(
						AbstractIntegerAssert::isZero),

				compareBuilding(Builder::withAbsentProduct, Builder::withPresentProduct).andThen(
						AbstractIntegerAssert::isNegative),
				compareBuilding(Builder::withPresentProduct, Builder::withAbsentProduct).andThen(
						AbstractIntegerAssert::isPositive),
				compareBuilding(Builder::withAbsentProduct, Builder::withAbsentProduct).andThen(AbstractIntegerAssert::isZero),
				compareBuilding(Builder::withPresentProduct, Builder::withPresentProduct).andThen(AbstractIntegerAssert::isZero),

				compareBuilding(Builder::withAbsentPg, Builder::withPresentPg).andThen(AbstractIntegerAssert::isNegative),
				compareBuilding(Builder::withPresentPg, Builder::withAbsentPg).andThen(AbstractIntegerAssert::isPositive),
				compareBuilding(Builder::withAbsentPg, Builder::withAbsentPg).andThen(AbstractIntegerAssert::isZero),
				compareBuilding(Builder::withPresentPg, Builder::withPresentPg).andThen(AbstractIntegerAssert::isZero),

				compareBuilding(left -> withTaxCode(left, "ab123"), right -> withTaxCode(right, "AB234"))
						.andThen(AbstractIntegerAssert::isNegative),
				compareBuilding(left -> withTaxCode(left, "ab234"), right -> withTaxCode(right, "ab123"))
						.andThen(AbstractIntegerAssert::isPositive),
				compareBuilding(left -> withTaxCode(left, "theSame"), right -> withTaxCode(right, "theSame"))
						.andThen(AbstractIntegerAssert::isZero),

				compareBuilding(left -> left.withPK(PK.NULL_PK), right -> right.withPK(PK.BIG_PK)).andThen(
						AbstractIntegerAssert::isNegative),
				compareBuilding(left -> left.withPK(PK.BIG_PK), right -> right.withPK(PK.NULL_PK)).andThen(
						AbstractIntegerAssert::isPositive),
				compareBuilding(left -> left.withPK(PK.NULL_PK), right -> right.withPK(PK.NULL_PK)).andThen(
						AbstractIntegerAssert::isZero));
	}

	static Function<Void, AbstractIntegerAssert> compareBuilding(
			final Function<Builder<TaxRowModel>, Builder<TaxRowModel>> functionA,
			final Function<Builder<TaxRowModel>, Builder<TaxRowModel>> functionB)
	{
		final TaxRowModel builtModelA = functionA.andThen(Builder::build).apply(Builder.of(TaxRowModel.class));
		final TaxRowModel builtModelB = functionB.andThen(Builder::build).apply(Builder.of(TaxRowModel.class));
		return compare(builtModelA, builtModelB);
	}

	static Function<Void, AbstractIntegerAssert> compare(final TaxRowModel builtModelA, final TaxRowModel builtModelB)
	{
		return ignored -> {
			final Comparator<TaxRowModel> comparator = new ProductEurope1TaxesAttributeHandler().getPdtRowComparator();
			return assertThat(comparator.compare(builtModelA, builtModelB));
		};
	}

	static Builder<TaxRowModel> withTaxCode(final Builder<TaxRowModel> builder, final String code)
	{
		return builder.with(model -> {
			final TaxModel taxMock = mock(TaxModel.class);
			given(taxMock.getCode()).willReturn(code);
			given(model.getTax()).willReturn(taxMock);
		});
	}
}
