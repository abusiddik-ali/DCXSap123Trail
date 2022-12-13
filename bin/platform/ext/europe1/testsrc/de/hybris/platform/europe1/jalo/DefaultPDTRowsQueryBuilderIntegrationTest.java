/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.europe1.jalo;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.europe1.constants.Europe1Constants;
import de.hybris.platform.europe1.jalo.PDTRowsQueryBuilder.QueryWithParams;
import de.hybris.platform.jalo.JaloItemNotFoundException;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.jalo.SessionContext;
import de.hybris.platform.jalo.c2l.C2LManager;
import de.hybris.platform.jalo.c2l.Currency;
import de.hybris.platform.jalo.enumeration.EnumerationManager;
import de.hybris.platform.jalo.enumeration.EnumerationType;
import de.hybris.platform.jalo.enumeration.EnumerationValue;
import de.hybris.platform.jalo.flexiblesearch.FlexibleSearch;
import de.hybris.platform.jalo.product.Product;
import de.hybris.platform.jalo.product.ProductManager;
import de.hybris.platform.jalo.product.Unit;
import de.hybris.platform.jalo.type.ComposedType;
import de.hybris.platform.jalo.user.User;
import de.hybris.platform.jalo.user.UserManager;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.util.Collection;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;


@SuppressWarnings("deprecation")
@IntegrationTest
public class DefaultPDTRowsQueryBuilderIntegrationTest extends ServicelayerBaseTest
{
	private static final String TEST_PRODUCT = "TEST_PRODUCT";
	private static final String TEST_USER = "TEST_USER";
	private static final String TEST_USER_GROUP = "TEST_USER_GROUP";
	private static final String TEST_PRODUCT_GROUP = "TEST_PRODUCT_GROUP";

	private final PropertyConfigSwitcher productIdLegacySwitcher = new PropertyConfigSwitcher(
			DefaultPDTRowsQueryBuilder.USE_LEGACY_PRODUCTID_QUERY_STRATEGY);

	private Europe1PriceFactory factory;
	private Unit unit;
	private Currency currency;

	private PriceRow anyAny;
	private PriceRow anyGiven;
	private PriceRow anyGroup;
	private PriceRow givenAny;
	private PriceRow givenGiven;
	private PriceRow givenGroup;
	private PriceRow groupAny;
	private PriceRow groupGiven;
	private PriceRow groupGroup;
	private PriceRow idAny;
	private PriceRow idGiven;
	private PriceRow idGroup;
	private Product anyProduct;
	private Product givenProduct;
	private EnumerationValue givenProductGroup;
	private User anyUser;
	private User givenUser;
	private EnumerationValue givenUserGroup;

	@Before
	public void setUp() throws Exception
	{
		productIdLegacySwitcher.switchToValue("false");

		factory = Europe1PriceFactory.getInstance();
		final ProductManager productManager = ProductManager.getInstance();
		final UserManager userManager = UserManager.getInstance();
		final EnumerationManager enumerationManager = EnumerationManager.getInstance();
		final EnumerationType productGroupType = enumerationManager.getEnumerationType(Europe1Constants.TYPES.PRICE_PRODUCT_GROUP);

		unit = productManager.createUnit("pieces", "pieces");
		try
		{
			currency = C2LManager.getInstance().getCurrencyByIsoCode("EUR");
		}
		catch (final JaloItemNotFoundException e)
		{
			currency = C2LManager.getInstance().createCurrency("EUR");
		}

		productManager.createProduct(TEST_PRODUCT);
		userManager.createCustomer(TEST_USER);
		factory.createUserPriceGroup(TEST_USER_GROUP);
		enumerationManager.createEnumerationValue(productGroupType, TEST_PRODUCT_GROUP);

		anyProduct = product(null);
		givenProduct = product(TEST_PRODUCT);
		givenProductGroup = productGroup(TEST_PRODUCT_GROUP);
		anyUser = user(null);
		givenUser = user(TEST_USER);
		givenUserGroup = userGroup(TEST_USER_GROUP);

		anyAny = createPrice(anyProduct, anyUser);
		anyGiven = createPrice(anyProduct, givenUser);
		anyGroup = createPrice(anyProduct, givenUserGroup);
		givenAny = createPrice(givenProduct, anyUser);
		givenGiven = createPrice(givenProduct, givenUser);
		givenGroup = createPrice(givenProduct, givenUserGroup);
		groupAny = createPrice(givenProductGroup, anyUser);
		groupGiven = createPrice(givenProductGroup, givenUser);
		groupGroup = createPrice(givenProductGroup, givenUserGroup);
		idAny = createPrice(TEST_PRODUCT, anyUser);
		idGiven = createPrice(TEST_PRODUCT, givenUser);
		idGroup = createPrice(TEST_PRODUCT, givenUserGroup);
	}

	@After
	public void cleanUp()
	{
		productIdLegacySwitcher.switchBackToDefault();
	}

	@Test
	public void shouldQueryPricesForEmptyParameters()
	{
		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);

		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(anyAny, anyGiven, anyGroup, givenAny, givenGiven, givenGroup, groupAny, groupGiven,
				groupGroup, idAny, idGiven, idGroup);
	}

	@Test
	public void shouldQueryPricesForEmptyParametersLegacy()
	{
		productIdLegacySwitcher.switchToValue("true");

		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);

		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(anyAny, anyGiven, anyGroup, givenAny, givenGiven, givenGroup, groupAny, groupGiven,
				groupGroup, idAny, idGiven, idGroup);
	}

	@Test
	public void shouldQueryPricesForAnyProductAndAnyUser()
	{
		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withAnyProduct()//
				.withAnyUser();

		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(anyAny);
	}

	@Test
	public void shouldQueryPricesForAnyProductAndGivenUser()
	{
		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withAnyProduct()///
				.withUser(givenUser.getPK());

		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(anyGiven);
	}

	@Test
	public void shouldQueryPricesForAnyProductAndGivenUserGroup()
	{
		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withAnyProduct()//
				.withUserGroup(givenUserGroup.getPK());

		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(anyGroup);
	}

	@Test
	public void shouldQueryPricesForGivenProductAndAnyUser()
	{
		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withAnyUser()//
				.withProduct(givenProduct.getPK());

		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(givenAny);
	}

	@Test
	public void shouldQueryPricesForGivenProductAndGivenUser()
	{
		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withProduct(givenProduct.getPK())//
				.withUser(givenUser.getPK());

		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(givenGiven);
	}

	@Test
	public void shouldQueryPricesForGivenProductGroupAndAnyUser()
	{
		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withAnyUser()//
				.withProductGroup(givenProductGroup.getPK());

		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(groupAny);
	}

	@Test
	public void shouldQueryPricesForGivenProductGroupAndGivenUser()
	{
		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withProductGroup(givenProductGroup.getPK())//
				.withUser(givenUser.getPK());

		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(groupGiven);
	}

	@Test
	public void shouldQueryPricesForGivenProductGroupAndGivenUserGroup()
	{
		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withProductGroup(givenProductGroup.getPK())//
				.withUserGroup(givenUserGroup.getPK());

		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(groupGroup);
	}

	@Test
	public void shouldQueryPricesForGivenProductProductIdAndGivenUserGroup()
	{
		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withProduct(givenProduct.getPK())//
				.withProductId(TEST_PRODUCT)//
				.withUserGroup(givenUserGroup.getPK());

		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(givenGroup, idGroup);
	}

	@Test
	public void shouldQueryPricesForGivenProductIdAndGivenUserGroup()
	{
		productIdLegacySwitcher.switchToValue("false");

		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withProductId(TEST_PRODUCT)//
				.withUserGroup(givenUserGroup.getPK());
		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(idGroup);
	}

	@Test
	public void shouldQueryPricesForGivenProductIdAndGivenUserGroupLegacy()
	{
		productIdLegacySwitcher.switchToValue("true");

		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withProductId(TEST_PRODUCT)//
				.withUserGroup(givenUserGroup.getPK());
		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(anyGroup, givenGroup, groupGroup, idGroup);
	}

	@Test
	public void shouldQueryPricesForAnyProductGivenProductIdAndGivenUserGroup()
	{
		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withAnyProduct()//
				.withProductId(TEST_PRODUCT)//
				.withUserGroup(givenUserGroup.getPK());
		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(anyGroup, idGroup);
	}

	@Test
	public void shouldQueryPricesForAnyUserGivenProductIdAndGivenUserGroup()
	{
		productIdLegacySwitcher.switchToValue("false");

		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withAnyUser()//
				.withProductId(TEST_PRODUCT)//
				.withUserGroup(givenUserGroup.getPK());
		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(idAny, idGroup);
	}

	@Test
	public void shouldQueryPricesForAnyUserGivenProductIdAndGivenUserGroupLegacy()
	{
		productIdLegacySwitcher.switchToValue("true");

		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withAnyUser()//
				.withProductId(TEST_PRODUCT)//
				.withUserGroup(givenUserGroup.getPK());
		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(anyAny, anyGroup, givenAny, givenGroup, groupAny, groupGroup, idAny, idGroup);
	}

	@Test
	public void shouldQueryPricesForAnyProductAnyUserGivenProductIdAndGivenUserGroup()
	{
		productIdLegacySwitcher.switchToValue("true");

		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withAnyProduct()//
				.withAnyUser()//
				.withProductId(TEST_PRODUCT)//
				.withUserGroup(givenUserGroup.getPK());
		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(anyAny, anyGroup, idAny, idGroup);
	}

	@Test
	public void shouldQueryPricesForGivenProductProductIdAndGivenProductGroup()
	{
		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withProduct(givenProduct.getPK())//
				.withProductId(TEST_PRODUCT)//
				.withProductGroup(givenProductGroup.getPK());

		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(givenGroup, givenAny, givenGiven, groupAny, groupGiven, groupGroup, idGroup, idAny,
				idGiven);
	}


	@Test
	public void shouldQueryPricesForGivenProductId()
	{
		final PDTRowsQueryBuilder builder = PDTRowsQueryBuilder.defaultBuilder(Europe1Constants.TC.PRICEROW);
		builder//
				.withProductId(TEST_PRODUCT);

		final Collection<PriceRow> prices = queryForPrices(builder);

		assertThat(prices).containsOnly(idAny, idGiven, idGroup);
	}

	private Collection<PriceRow> queryForPrices(final PDTRowsQueryBuilder builder)
	{
		final QueryWithParams queryAndParams = builder.build();

		return FlexibleSearch.getInstance().search(null, queryAndParams.getQuery(), queryAndParams.getParams(), PriceRow.class)
				.getResult();
	}

	private PriceRow createPrice(final Object product, final Object user) throws Exception
	{
		final Product prod = (product instanceof Product) ? (Product) product : null;
		final EnumerationValue prodGroup = (product instanceof EnumerationValue) ? (EnumerationValue) product : null;
		final User usr = (user instanceof User) ? (User) user : null;
		final EnumerationValue usrGroup = (user instanceof EnumerationValue) ? (EnumerationValue) user : null;
		final String productCode = (product instanceof String) ? (String) product : null;

		final SessionContext ctx = JaloSession.getCurrentSession().getSessionContext();

		return (PriceRow) ComposedType.newInstance(ctx, PriceRow.class, PriceRow.PRODUCT, prod, PriceRow.PG, prodGroup,
				PriceRow.USER, usr, PriceRow.UG, usrGroup, PriceRow.MINQTD, Long.valueOf(2), PriceRow.CURRENCY, currency,
				PriceRow.UNIT, unit, PriceRow.UNITFACTOR, Integer.valueOf(1), PriceRow.NET, Boolean.TRUE, PriceRow.DATERANGE, null,
				PriceRow.PRICE, Double.valueOf(123.45), PriceRow.PRODUCTID, productCode);
	}

	private User user(final String login)
	{
		if (login == null)
		{
			return null;
		}
		return UserManager.getInstance().getUserByLogin(login);
	}

	private EnumerationValue userGroup(final String code)
	{
		if (code == null)
		{
			return null;
		}

		return factory.getUserPriceGroup(code);
	}

	private Product product(final String code)
	{
		if (code == null)
		{
			return null;
		}

		final Collection candidates = ProductManager.getInstance().getProductsByCode(code);
		if (candidates == null || candidates.isEmpty())
		{
			return null;
		}
		if (candidates.size() > 1)
		{
			throw new IllegalStateException("More than one product for code " + code + " have been found.");
		}
		return (Product) candidates.iterator().next();
	}

	private EnumerationValue productGroup(final String code)
	{
		if (code == null)
		{
			return null;
		}

		final EnumerationManager manager = EnumerationManager.getInstance();
		final EnumerationType type = manager.getEnumerationType(Europe1Constants.TYPES.PRICE_PRODUCT_GROUP);

		return manager.getEnumerationValue(type, code);
	}
}
