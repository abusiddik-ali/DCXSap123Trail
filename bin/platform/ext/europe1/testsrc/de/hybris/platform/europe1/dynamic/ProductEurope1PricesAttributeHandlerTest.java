/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.europe1.dynamic;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;

import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.product.UnitModel;
import de.hybris.platform.europe1.model.PriceRowModel;

import java.util.Arrays;
import java.util.Comparator;
import java.util.function.Function;

import org.assertj.core.api.AbstractIntegerAssert;
import org.junit.runners.Parameterized;


public class ProductEurope1PricesAttributeHandlerTest extends AbstractProductEurope1AttributeHandlerTest
{

	public ProductEurope1PricesAttributeHandlerTest(final Function<Void, AbstractIntegerAssert> function)
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

				compareBuilding(left -> left.withPdtRowCode("ab123"), right -> right.withPdtRowCode("AB234"))
						.andThen(AbstractIntegerAssert::isNegative),
				compareBuilding(left -> left.withPdtRowCode("ab234"), right -> right.withPdtRowCode("AB123"))
						.andThen(AbstractIntegerAssert::isPositive),
				compareBuilding(left -> left.withPdtRowCode("theSame"), right -> right.withPdtRowCode("theSame"))
						.andThen(AbstractIntegerAssert::isZero),

				compareBuilding(Builder::withAbsentProduct, Builder::withPresentProduct).andThen(
						AbstractIntegerAssert::isNegative),
				compareBuilding(Builder::withPresentProduct, Builder::withAbsentProduct).andThen(
						AbstractIntegerAssert::isPositive),
				compareBuilding(Builder::withPresentProduct, Builder::withPresentProduct).andThen(AbstractIntegerAssert::isZero),
				compareBuilding(Builder::withAbsentProduct, Builder::withAbsentProduct).andThen(AbstractIntegerAssert::isZero),

				compareBuilding(Builder::withAbsentProductId, Builder::withPresentProductId).andThen(
						AbstractIntegerAssert::isNegative),
				compareBuilding(Builder::withPresentProductId, Builder::withAbsentProductId).andThen(
						AbstractIntegerAssert::isPositive),
				compareBuilding(Builder::withAbsentProductId, Builder::withAbsentProduct).andThen(AbstractIntegerAssert::isZero),
				compareBuilding(Builder::withPresentProductId, Builder::withPresentProductId).andThen(
						AbstractIntegerAssert::isZero),

				compareBuilding(Builder::withAbsentPg, Builder::withPresentPg).andThen(AbstractIntegerAssert::isNegative),
				compareBuilding(Builder::withPresentPg, Builder::withAbsentPg).andThen(AbstractIntegerAssert::isPositive),
				compareBuilding(Builder::withAbsentPg, Builder::withAbsentPg).andThen(AbstractIntegerAssert::isZero),
				compareBuilding(Builder::withPresentPg, Builder::withPresentPg).andThen(AbstractIntegerAssert::isZero),

				compareBuilding(left -> withCurrencyIsoCode(left, "ab123"),
						right -> withCurrencyIsoCode(right, "AB234"))
						.andThen(AbstractIntegerAssert::isNegative),
				compareBuilding(left -> withCurrencyIsoCode(left, "ab234"),
						right -> withCurrencyIsoCode(right, "AB123"))
						.andThen(AbstractIntegerAssert::isPositive),
				compareBuilding(left -> withCurrencyIsoCode(left, "theSame"),
						right -> withCurrencyIsoCode(right, "theSame"))
						.andThen(AbstractIntegerAssert::isZero),

				compareBuilding(ProductEurope1PricesAttributeHandlerTest::withAbsentNet,
						ProductEurope1PricesAttributeHandlerTest::withPresentNet).andThen(
						AbstractIntegerAssert::isNegative),
				compareBuilding(ProductEurope1PricesAttributeHandlerTest::withPresentNet,
						ProductEurope1PricesAttributeHandlerTest::withAbsentNet).andThen(
						AbstractIntegerAssert::isPositive),
				compareBuilding(ProductEurope1PricesAttributeHandlerTest::withAbsentNet,
						ProductEurope1PricesAttributeHandlerTest::withAbsentNet).andThen(AbstractIntegerAssert::isZero),
				compareBuilding(ProductEurope1PricesAttributeHandlerTest::withPresentNet,
						ProductEurope1PricesAttributeHandlerTest::withPresentNet).andThen(AbstractIntegerAssert::isZero),

				compareBuilding(left -> withUnitCode(left, "ab123"), right -> withUnitCode(right, "AB234"))
						.andThen(AbstractIntegerAssert::isNegative),
				compareBuilding(left -> withUnitCode(left, "ab234"), right -> withUnitCode(right, "AB123"))
						.andThen(AbstractIntegerAssert::isPositive),
				compareBuilding(left -> withUnitCode(left, "theSame"), right -> withUnitCode(right, "theSame"))
						.andThen(AbstractIntegerAssert::isZero),

				compareBuilding(left -> withMinqtd(left, 1L), right -> withMinqtd(right, 2L)).andThen(
						AbstractIntegerAssert::isNegative),
				compareBuilding(left -> withMinqtd(left, 2L), right -> withMinqtd(right, 1L)).andThen(
						AbstractIntegerAssert::isPositive),
				compareBuilding(left -> withMinqtd(left, 3L), right -> withMinqtd(right, 3L)).andThen(
						AbstractIntegerAssert::isZero),

				compareBuilding(left -> left.withPK(PK.NULL_PK), right -> right.withPK(PK.BIG_PK)).andThen(
						AbstractIntegerAssert::isNegative),
				compareBuilding(left -> left.withPK(PK.BIG_PK), right -> right.withPK(PK.NULL_PK)).andThen(
						AbstractIntegerAssert::isPositive),
				compareBuilding(left -> left.withPK(PK.NULL_PK), right -> right.withPK(PK.NULL_PK)).andThen(
						AbstractIntegerAssert::isZero));
	}

	static Function<Void, AbstractIntegerAssert> compareBuilding(
			final Function<Builder<PriceRowModel>, Builder<PriceRowModel>> functionA,
			final Function<Builder<PriceRowModel>, Builder<PriceRowModel>> functionB)
	{
		final PriceRowModel builtModelA = functionA.andThen(Builder::build).apply(Builder.of(PriceRowModel.class));
		final PriceRowModel builtModelB = functionB.andThen(Builder::build).apply(Builder.of(PriceRowModel.class));
		return compare(builtModelA, builtModelB);
	}

	static Function<Void, AbstractIntegerAssert> compare(final PriceRowModel builtModelA, final PriceRowModel builtModelB)
	{
		return ignored -> {
			final Comparator<PriceRowModel> comparator = new ProductEurope1PricesAttributeHandler().getPdtRowComparator();
			return assertThat(comparator.compare(builtModelA, builtModelB));
		};
	}

	static Builder<PriceRowModel> withCurrencyIsoCode(final Builder<PriceRowModel> builder, final String isoCode)
	{
		return builder.with(model -> {
			final CurrencyModel currencyMock = mock(CurrencyModel.class);
			given(currencyMock.getIsocode()).willReturn(isoCode);
			given(model.getCurrency()).willReturn(currencyMock);
		});
	}

	static Builder<PriceRowModel> withPresentNet(final Builder<PriceRowModel> builder)
	{
		return builder.with(model -> given(model.getNet()).willReturn(true));
	}

	static Builder<PriceRowModel> withAbsentNet(final Builder<PriceRowModel> builder)
	{
		return builder.with(model -> given(model.getNet()).willReturn(null));
	}

	static Builder<PriceRowModel> withUnitCode(final Builder<PriceRowModel> builder, final String code)
	{
		return builder.with(model -> {
			final UnitModel unitMock = mock(UnitModel.class);
			given(unitMock.getCode()).willReturn(code);
			given(model.getUnit()).willReturn(unitMock);
		});
	}

	static Builder<PriceRowModel> withMinqtd(final Builder<PriceRowModel> builder, final Long minqtd)
	{
		return builder.with(model -> given(model.getMinqtd()).willReturn(minqtd));
	}
}
