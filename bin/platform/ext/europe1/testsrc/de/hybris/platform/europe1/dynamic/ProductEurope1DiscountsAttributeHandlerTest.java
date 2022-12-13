/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.europe1.dynamic;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;

import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.order.price.DiscountModel;
import de.hybris.platform.europe1.model.DiscountRowModel;

import java.util.Arrays;
import java.util.Comparator;
import java.util.function.Function;

import org.assertj.core.api.AbstractIntegerAssert;
import org.junit.runners.Parameterized;


public class ProductEurope1DiscountsAttributeHandlerTest extends AbstractProductEurope1AttributeHandlerTest
{

	public ProductEurope1DiscountsAttributeHandlerTest(final Function<Void, AbstractIntegerAssert> function)
	{
		super(function);
	}

	@Parameterized.Parameters
	public static Iterable<Object> data()
	{
		return Arrays.asList( //
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
				compareBuilding(Builder::withPresentProduct, Builder::withPresentProduct).andThen(
						AbstractIntegerAssert::isZero),

				compareBuilding(Builder::withAbsentProductId, Builder::withPresentProductId).andThen(
						AbstractIntegerAssert::isNegative),
				compareBuilding(Builder::withPresentProductId, Builder::withAbsentProductId).andThen(
						AbstractIntegerAssert::isPositive),
				compareBuilding(Builder::withAbsentProductId, Builder::withAbsentProductId).andThen(
						AbstractIntegerAssert::isZero),
				compareBuilding(Builder::withPresentProductId, Builder::withPresentProductId).andThen(
						AbstractIntegerAssert::isZero),

				compareBuilding(Builder::withAbsentPg, Builder::withPresentPg).andThen(AbstractIntegerAssert::isNegative),
				compareBuilding(Builder::withPresentPg, Builder::withAbsentPg).andThen(AbstractIntegerAssert::isPositive),
				compareBuilding(Builder::withAbsentPg, Builder::withAbsentPg).andThen(AbstractIntegerAssert::isZero),
				compareBuilding(Builder::withPresentPg, Builder::withPresentPg).andThen(AbstractIntegerAssert::isZero),

				compareBuilding(left -> withDiscountCode(left, "ab123"), right -> withDiscountCode(right, "AB234"))
						.andThen(AbstractIntegerAssert::isNegative),
				compareBuilding(left -> withDiscountCode(left, "ab234"), right -> withDiscountCode(right, "AB123"))
						.andThen(AbstractIntegerAssert::isPositive),
				compareBuilding(left -> withDiscountCode(left, "theSame"),
						right -> withDiscountCode(right, "theSame"))
						.andThen(AbstractIntegerAssert::isZero),

				compareBuilding(left -> withCurrencyIsoCode(left, "ab123"),
						right -> withCurrencyIsoCode(right, "AB234"))
						.andThen(AbstractIntegerAssert::isNegative),
				compareBuilding(left -> withCurrencyIsoCode(left, "ab234"),
						right -> withCurrencyIsoCode(right, "AB123"))
						.andThen(AbstractIntegerAssert::isPositive),
				compareBuilding(left -> withCurrencyIsoCode(left, "theSame"),
						right -> withCurrencyIsoCode(right, "theSame"))
						.andThen(AbstractIntegerAssert::isZero),

				compareBuilding(left -> left.withPK(PK.NULL_PK), right -> right.withPK(PK.BIG_PK)).andThen(
						AbstractIntegerAssert::isNegative),
				compareBuilding(left -> left.withPK(PK.BIG_PK), right -> right.withPK(PK.NULL_PK)).andThen(
						AbstractIntegerAssert::isPositive),
				compareBuilding(left -> left.withPK(PK.NULL_PK), right -> right.withPK(PK.NULL_PK)).andThen(
						AbstractIntegerAssert::isZero));
	}

	static Function<Void, AbstractIntegerAssert> compareBuilding(
			final Function<Builder<DiscountRowModel>, Builder<DiscountRowModel>> functionA,
			final Function<Builder<DiscountRowModel>, Builder<DiscountRowModel>> functionB)
	{
		final DiscountRowModel builtModelA = functionA.andThen(Builder::build).apply(Builder.of(DiscountRowModel.class));
		final DiscountRowModel builtModelB = functionB.andThen(Builder::build).apply(Builder.of(DiscountRowModel.class));
		return compare(builtModelA, builtModelB);
	}

	static Function<Void, AbstractIntegerAssert> compare(final DiscountRowModel builtModelA, final DiscountRowModel builtModelB)
	{
		return ignored -> {
			final Comparator<DiscountRowModel> comparator = new ProductEurope1DiscountsAttributeHandler().getPdtRowComparator();
			return assertThat(comparator.compare(builtModelA, builtModelB));
		};
	}

	static Builder<DiscountRowModel> withCurrencyIsoCode(final Builder<DiscountRowModel> builder, final String code)
	{
		return builder.with(model -> {
			final CurrencyModel currencyMock = mock(CurrencyModel.class);
			given(currencyMock.getIsocode()).willReturn(code);
			given(model.getCurrency()).willReturn(currencyMock);
		});
	}

	static Builder<DiscountRowModel> withDiscountCode(final Builder<DiscountRowModel> builder, final String code)
	{
		return builder.with(model -> {
			final DiscountModel discountMock = mock(DiscountModel.class);
			given(discountMock.getCode()).willReturn(code);
			given(model.getDiscount()).willReturn(discountMock);
		});
	}
}
