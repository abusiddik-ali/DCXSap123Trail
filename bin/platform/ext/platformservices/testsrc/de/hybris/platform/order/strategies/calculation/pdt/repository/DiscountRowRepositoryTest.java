/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.calculation.pdt.repository;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.order.price.DiscountModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.europe1.enums.ProductDiscountGroup;
import de.hybris.platform.europe1.enums.UserDiscountGroup;
import de.hybris.platform.europe1.model.AbstractDiscountRowModel;
import de.hybris.platform.europe1.model.DiscountRowModel;
import de.hybris.platform.europe1.model.GlobalDiscountRowModel;
import de.hybris.platform.jalo.order.OrderManager;
import de.hybris.platform.jalo.order.price.JaloPriceFactoryException;
import de.hybris.platform.jalo.product.ProductManager;
import de.hybris.platform.jalo.user.UserManager;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.DiscountValueInfoCriteria;
import de.hybris.platform.order.strategies.calculation.pdt.criteria.impl.DefaultDiscountValueInfoCriteria;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.servicelayer.ServicelayerTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.util.Collection;
import java.util.Date;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;


@SuppressWarnings("deprecation")
@IntegrationTest
public class DiscountRowRepositoryTest extends ServicelayerTest
{
	private static final String TEST_PRODUCT = "TEST_PRODUCT";
	private static final String TEST_USER = "TEST_USER";
	private static final String TEST_USER_GROUP = "TEST_USER_GROUP";
	private static final String TEST_PRODUCT_GROUP = "TEST_PRODUCT_GROUP";
	private final PropertyConfigSwitcher persistenceLegacyModeSwitch = new PropertyConfigSwitcher("persistence.legacy.mode");
	@Resource
	private ModelService modelService;
	@Resource
	private UserService userService;
	@Resource
	private ProductService productService;
	@Resource
	private PDTRowRepository<DiscountValueInfoCriteria, DiscountRowModel> defaultDiscountRowRepository;
	private DiscountModel discount;
	private CurrencyModel currency;
	private AbstractDiscountRowModel any_any;
	private AbstractDiscountRowModel any_given;
	private AbstractDiscountRowModel any_group;
	private AbstractDiscountRowModel given_any;
	private AbstractDiscountRowModel given_given;
	private AbstractDiscountRowModel given_group;
	private AbstractDiscountRowModel group_any;
	private AbstractDiscountRowModel group_given;
	private AbstractDiscountRowModel group_group;
	private AbstractDiscountRowModel global_any;
	private AbstractDiscountRowModel global_given;
	private AbstractDiscountRowModel global_group;
	private AbstractDiscountRowModel id_any;
	private AbstractDiscountRowModel id_given;
	private AbstractDiscountRowModel id_group;
	private ProductModel anyProduct;
	private ProductModel givenProduct;
	private ProductDiscountGroup givenProductGroup;
	private UserModel anyUser;
	private UserModel givenUser;
	private UserDiscountGroup givenUserGroup;
	private CatalogModel testCatalog;
	private CatalogVersionModel testCatalogVersion;

	@Before
	public void setUp() throws Exception
	{
		createCoreData();
		final ProductManager productManager = ProductManager.getInstance();
		final UserManager userManager = UserManager.getInstance();
		final OrderManager orderManager = OrderManager.getInstance();

		testCatalog = modelService.create(CatalogModel.class);
		testCatalog.setId("testCatalog2");
		modelService.save(testCatalog);

		testCatalogVersion = modelService.create(CatalogVersionModel.class);
		testCatalogVersion.setCatalog(testCatalog);
		testCatalogVersion.setVersion("online");

		discount = modelService.create(DiscountModel.class);
		discount.setCode("TEST_DISCOUNT");
		modelService.save(discount);

		currency = modelService.get(getOrCreateCurrency("EUR"));

		productManager.createProduct(TEST_PRODUCT);

		userManager.createCustomer(TEST_USER);
		givenUserGroup = UserDiscountGroup.valueOf(TEST_USER_GROUP);
		modelService.save(givenUserGroup);

		givenProductGroup = ProductDiscountGroup.valueOf(TEST_PRODUCT_GROUP);
		modelService.save(givenProductGroup);

		anyProduct = null;
		givenProduct = product(TEST_PRODUCT);

		anyUser = user(null);
		givenUser = user(TEST_USER);

		any_any = createDiscount(anyProduct, anyUser);
		any_given = createDiscount(anyProduct, givenUser);
		any_group = createDiscount(anyProduct, givenUserGroup);
		given_any = createDiscount(givenProduct, anyUser);
		given_given = createDiscount(givenProduct, givenUser);
		given_group = createDiscount(givenProduct, givenUserGroup);
		group_any = createDiscount(givenProductGroup, anyUser);
		group_given = createDiscount(givenProductGroup, givenUser);
		group_group = createDiscount(givenProductGroup, givenUserGroup);
		global_any = createGlobalDiscount(anyUser);
		global_given = createGlobalDiscount(givenUser);
		global_group = createGlobalDiscount(givenUserGroup);
		id_any = createDiscount(TEST_PRODUCT, anyUser);
		id_given = createDiscount(TEST_PRODUCT, givenUser);
		id_group = createDiscount(TEST_PRODUCT, givenUserGroup);
	}

	@After
	public void tearDown()
	{
		persistenceLegacyModeSwitch.switchBackToDefault();
	}

	@Test
	public void shouldQueryDiscountsForAnyProductAndAnyUser()
	{
		final Collection<? extends AbstractDiscountRowModel> prices = queryForDiscounts(anyProduct, anyUser);

		assertThat(prices).extracting(ItemModel::getPk).containsOnly(global_any.getPk());
	}

	@Test
	public void shouldQueryDiscountsForAnyProductAndGivenUser()
	{
		final Collection<? extends AbstractDiscountRowModel> prices = queryForDiscounts(anyProduct, givenUser);

		assertThat(prices).extracting(ItemModel::getPk).containsOnly(global_any.getPk(), global_given.getPk());
	}

	@Test
	public void shouldQueryDiscountsForAnyProductAndGivenUserGroup()
	{
		final Collection<? extends AbstractDiscountRowModel> prices = queryForDiscounts(anyProduct, givenUserGroup);

		assertThat(prices).extracting(ItemModel::getPk).containsOnly(global_any.getPk(), global_group.getPk());
	}

	@Test
	public void shouldQueryDiscountsForGivenProductAndAnyUser()
	{
		final Collection<? extends AbstractDiscountRowModel> prices = queryForDiscounts(givenProduct, anyUser);

		assertThat(prices).extracting(ItemModel::getPk).containsOnly(any_any.getPk(), given_any.getPk(), id_any.getPk());
	}

	@Test
	public void shouldQueryDiscountsForGivenProductAndGivenUser()
	{
		final Collection<? extends AbstractDiscountRowModel> prices = queryForDiscounts(givenProduct, givenUser);

		assertThat(prices).extracting(ItemModel::getPk)
		                  .containsOnly(any_any.getPk(), any_given.getPk(), given_any.getPk(), given_given.getPk(),
				                  id_any.getPk(), id_given.getPk());
	}

	@Test
	public void shouldQueryDiscountsForGivenProductAndGivenUserGroup()
	{
		final Collection<? extends AbstractDiscountRowModel> prices = queryForDiscounts(givenProduct, givenUserGroup);

		assertThat(prices).extracting(ItemModel::getPk)
		                  .containsOnly(any_any.getPk(), any_group.getPk(), given_any.getPk(), given_group.getPk(),
				                  id_any.getPk(), id_group.getPk());
	}

	@Test
	public void shouldQueryDiscountsForGivenProductGroupAndAnyUser()
	{
		final Collection<? extends AbstractDiscountRowModel> prices = queryForDiscounts(givenProductGroup, anyUser);

		assertThat(prices).extracting(ItemModel::getPk).containsOnly(any_any.getPk(), group_any.getPk());
	}

	@Test
	public void shouldQueryDiscountsForGivenProductGroupAndGivenUser()
	{
		final Collection<? extends AbstractDiscountRowModel> prices = queryForDiscounts(givenProductGroup, givenUser);

		assertThat(prices).extracting(ItemModel::getPk)
		                  .containsOnly(any_any.getPk(), any_given.getPk(), group_any.getPk(), group_given.getPk());
	}

	@Test
	public void shouldQueryDiscountsForGivenProductGroupAndGivenUserGroup()
	{
		final Collection<? extends AbstractDiscountRowModel> prices = queryForDiscounts(givenProductGroup, givenUserGroup);

		assertThat(prices).extracting(ItemModel::getPk)
		                  .containsOnly(any_any.getPk(), any_group.getPk(), group_any.getPk(), group_group.getPk());
	}

	private Collection<DiscountRowModel> queryForDiscounts(final Object product, final Object user)
	{
		final ProductModel prod = (product instanceof ProductModel) ? (ProductModel) product : null;
		final ProductDiscountGroup prodGroup = (product instanceof ProductDiscountGroup) ? (ProductDiscountGroup) product : null;
		final UserModel usr = (user instanceof UserModel) ? (UserModel) user : null;
		final UserDiscountGroup usrGroup = (user instanceof UserDiscountGroup) ? (UserDiscountGroup) user : null;

		final DiscountValueInfoCriteria criteria = DefaultDiscountValueInfoCriteria.buildForInfo() //
		                                                                           .withUser(usr) //
		                                                                           .withCurrency(currency) //
		                                                                           .withDate(new Date()) //
		                                                                           .withUserDiscountGroup(usrGroup) //
		                                                                           .withProduct(prod) //
		                                                                           .withProductDiscountGroup(prodGroup) //
		                                                                           .build();

		final Collection<DiscountRowModel> discountValues = defaultDiscountRowRepository.findRows(criteria);

		return discountValues;
	}

	private DiscountRowModel createDiscount(final Object product, final Object user) throws Exception
	{
		final ProductModel prod = (product instanceof ProductModel) ? (ProductModel) product : null;
		final ProductDiscountGroup prodGroup = (product instanceof ProductDiscountGroup) ? (ProductDiscountGroup) product : null;
		final UserModel usr = (user instanceof UserModel) ? (UserModel) user : null;
		final UserDiscountGroup usrGroup = (user instanceof UserDiscountGroup) ? (UserDiscountGroup) user : null;
		final String productId = (product instanceof String) ? (String) product : null;

		final DiscountRowModel ret = modelService.create(DiscountRowModel.class);
		ret.setProduct(prod);
		ret.setPg(prodGroup);
		ret.setProductId(productId);
		ret.setUser(usr);
		ret.setUg(usrGroup);
		ret.setCurrency(currency);
		ret.setValue(23.4);
		ret.setDiscount(discount);
		ret.setCatalogVersion(testCatalogVersion);
		modelService.save(ret);

		return ret;
	}

	private GlobalDiscountRowModel createGlobalDiscount(final Object user) throws JaloPriceFactoryException
	{
		final UserModel usr = (user instanceof UserModel) ? (UserModel) user : null;
		final UserDiscountGroup usrGroup = (user instanceof UserDiscountGroup) ? (UserDiscountGroup) user : null;

		final GlobalDiscountRowModel ret = modelService.create(GlobalDiscountRowModel.class);
		//		ret.setProduct(null);
		ret.setUser(usr);
		ret.setUg(usrGroup);
		ret.setCurrency(currency);
		ret.setValue(Double.valueOf(12.3));
		ret.setDiscount(discount);
		modelService.save(ret);

		return ret;
	}

	private UserModel user(final String login)
	{
		if (login == null)
		{
			return null;
		}

		return userService.getUserForUID(login);
	}

	private ProductModel product(final String code)
	{
		if (code == null)
		{
			return null;
		}

		return productService.getProductForCode(code);
	}
}
