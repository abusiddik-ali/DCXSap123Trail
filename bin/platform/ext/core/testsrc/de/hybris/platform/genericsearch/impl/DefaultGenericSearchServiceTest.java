/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.genericsearch.impl;

import static de.hybris.platform.testframework.Assert.assertCollectionElements;
import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNotNull;
import static junit.framework.Assert.assertNull;
import static junit.framework.Assert.assertTrue;
import static junit.framework.Assert.fail;
import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.Constants;
import de.hybris.platform.core.GenericCondition;
import de.hybris.platform.core.GenericConditionList;
import de.hybris.platform.core.GenericFunctionSelectField;
import de.hybris.platform.core.GenericQuery;
import de.hybris.platform.core.GenericSearchField;
import de.hybris.platform.core.GenericSearchFieldType;
import de.hybris.platform.core.GenericSearchOrderBy;
import de.hybris.platform.core.GenericSelectField;
import de.hybris.platform.core.Operator;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.c2l.LanguageModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.product.UnitModel;
import de.hybris.platform.core.model.security.PrincipalModel;
import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.genericsearch.GenericSearchQuery;
import de.hybris.platform.genericsearch.GenericSearchService;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.util.Config;

import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.annotation.Resource;

import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class DefaultGenericSearchServiceTest extends ServicelayerTransactionalBaseTest
{
	private static final Logger LOG = Logger.getLogger(DefaultGenericSearchServiceTest.class);
	private static final String NAME_TEST_01 = "name-test-01";

	@Resource
	private ModelService modelService;
	@Resource
	private CommonI18NService commonI18NService;
	@Resource
	private I18NService i18nService;
	@Resource
	private GenericSearchService genericSearchService;

	private LanguageModel german, english;
	private ProductModel product1, product2, product3, product4, product5;
	private UnitModel unit1;

	private CatalogVersionModel catalogVersionModel;
	private CatalogModel catalogModel;

	private Locale localeEn, localeDe;

	private final static String DE_ISOCODE = "de";
	private final static String EN_ISOCODE = "en";
	private final static String TEST_CATALOG_ID = "test_catalog";
	private final static String TEST_CATALOGVERSION_ID = "test_cv";

	@Before
	public void setUp() throws Exception
	{
		catalogModel = modelService.create(CatalogModel.class);
		catalogModel.setId(TEST_CATALOG_ID);
		modelService.save(catalogModel);
		catalogVersionModel = modelService.create(CatalogVersionModel.class);
		catalogVersionModel.setCatalog(catalogModel);
		catalogVersionModel.setVersion(TEST_CATALOGVERSION_ID);
		modelService.save(catalogVersionModel);

		try
		{
			german = commonI18NService.getLanguage(DE_ISOCODE);
		}
		catch (final UnknownIdentifierException e)
		{
			german = new LanguageModel();
			modelService.initDefaults(german);
			german.setIsocode(DE_ISOCODE);
			modelService.save(german);
		}

		try
		{
			english = commonI18NService.getLanguage(EN_ISOCODE);
		}
		catch (final UnknownIdentifierException e)
		{
			english = new LanguageModel();
			modelService.initDefaults(english);
			english.setIsocode(EN_ISOCODE);
			modelService.save(english);
		}

		localeEn = new Locale(EN_ISOCODE);
		localeDe = new Locale(DE_ISOCODE);
		i18nService.setCurrentLocale(localeEn);

		assertNotNull(product1 = makeProduct(1));
		assertNotNull(product2 = makeProduct(2));
		assertNotNull(product3 = makeProduct(3));
		assertNotNull(product4 = makeProduct(4));

		assertNotNull(unit1 = makeUnit("piece", "unit1"));
		modelService.save(unit1);
		product1.setUnit(unit1);
		modelService.save(product1);
		modelService.save(product2);
		modelService.save(product3);
		modelService.save(product4);
		product5 = modelService.create(ProductModel.class);
		product5.setCode("product5");
		product5.setCatalogVersion(catalogVersionModel);
		modelService.save(product5);
	}

	@Test
	public void searchGenericSearchQueryTest()
	{
		final GenericSearchField codeField = new GenericSearchField(ProductModel._TYPECODE, ProductModel.CODE);
		final GenericSearchField nameField = new GenericSearchField(ProductModel._TYPECODE, ProductModel.NAME);
		final GenericSelectField codeSelectField = new GenericSelectField(ProductModel._TYPECODE, ProductModel.CODE,
				String.class);
		final GenericSelectField nameSelectField = new GenericSelectField(ProductModel._TYPECODE, ProductModel.NAME,
				String.class);

		//---- test value condition /  condition list
		GenericCondition condition = GenericCondition.createConditionForValueComparison(codeField, Operator.EQUAL,
				product1.getCode());
		final GenericConditionList conditionList = GenericCondition.createConditionList(condition);
		conditionList.addToConditionList(GenericCondition.createConditionForValueComparison(nameField, Operator.STARTS_WITH,
				"pRoduCt", /* upper */true));
		GenericQuery query = new GenericQuery(ProductModel._TYPECODE);
		query.addCondition(conditionList);
		query.addSelectField(codeSelectField);
		query.addSelectField(nameSelectField);

		GenericSearchQuery gsquery = createGSQuery(query, true, german);

		final SearchResult<ArrayList<String>> searchResult = genericSearchService.search(gsquery);
		final List<ArrayList<String>> elements = searchResult.getResult();
		assertEquals(1, elements.size());
		final ArrayList<String> codes = elements.get(0);
		assertNotNull(codes);
		assertEquals(2, codes.size());
		assertTrue(product1.getCode().equals(codes.get(0)));
		// wrong operator
		try
		{
			condition.setOperator(Operator.IS_NULL);
			fail("Should have thrown an IllegalArgumentException.");
		}
		catch (final IllegalArgumentException e)
		{/* expected */
		}

		//---- test literal condition

		try
		// wrong operator
		{
			condition = GenericCondition.createConditionForLiteralComparison(codeField, Operator.GREATER);
			fail("Should have thrown an IllegalArgumentException.");
		}
		catch (final IllegalArgumentException e)
		{/* expected */
		}

		//	code field not all (should find all)
		condition = GenericCondition.createConditionForLiteralComparison(codeField, Operator.IS_NOT_NULL);
		query = new GenericQuery(ProductModel._TYPECODE);
		query.addCondition(condition);
		gsquery = createGSQuery(query, true, german);
		Collection result = genericSearchService.search(gsquery).getResult();
		assertEquals(5, result.size());

		//---- test field comparison condition

		try
		// wrong operator
		{
			condition = GenericCondition.createConditionForFieldComparison(codeField, Operator.IS_NOT_NULL, nameField);
			fail("Should have thrown an IllegalArgumentException.");
		}
		catch (final IllegalArgumentException e)
		{/* expected */
		}

		//	compare name field with code field (should find all)
		condition = GenericCondition.createConditionForFieldComparison(codeField, Operator.EQUAL, nameField);
		query = new GenericQuery(ProductModel._TYPECODE);
		query.addCondition(condition);
		gsquery = createGSQuery(query, true, german);
		result = genericSearchService.search(gsquery).getResult();
		assertEquals(4, result.size());

		// test function select MAX
		query = new GenericQuery(ProductModel._TYPECODE);
		query.addSelectField(new GenericFunctionSelectField(ProductModel.CODE, String.class, "MAX"));
		gsquery = createGSQuery(query, true, german);
		result = genericSearchService.search(gsquery).getResult();
		assertEquals(Collections.singletonList(product5.getCode()), result);

		// test function select MIN
		query = new GenericQuery(ProductModel._TYPECODE);
		query.addSelectField(new GenericFunctionSelectField(ProductModel.CODE, String.class, "MIN"));
		gsquery = createGSQuery(query, true, german);
		result = genericSearchService.search(gsquery).getResult();
		assertEquals(Collections.singletonList(product1.getCode()), result);

		// test function select COUNT
		query = new GenericQuery(ProductModel._TYPECODE);
		query.addSelectField(new GenericFunctionSelectField(ProductModel.PK, Integer.class, "COUNT"));
		gsquery = createGSQuery(query, true, german);
		result = genericSearchService.search(gsquery).getResult();
		assertEquals(Collections.singletonList(Integer.valueOf(5)), result);
	}

	@Test(expected = IllegalArgumentException.class)
	public void searchWithNullGQTest()
	{
		genericSearchService.search((GenericQuery) null);
	}

	@Test(expected = IllegalArgumentException.class)
	public void searchWithNullGSQTest()
	{
		genericSearchService.search((GenericSearchQuery) null);
	}

	@Test
	public void testGenericTypeJoin() throws Exception
	{
		final GenericSearchField productUnitField = new GenericSearchField(ProductModel._TYPECODE, ProductModel.UNIT);
		final GenericSearchField unitPkField = new GenericSearchField(UnitModel._TYPECODE, ItemModel.PK);
		final GenericSearchField unitCodeField = new GenericSearchField(UnitModel._TYPECODE, UnitModel.CODE);
		final GenericSearchField unitComposedTypeField = new GenericSearchField(UnitModel._TYPECODE, ItemModel.ITEMTYPE);
		final GenericSearchField composedTypePkField = new GenericSearchField(ComposedTypeModel._TYPECODE, ItemModel.PK);
		final GenericSearchField dumbOrderByField = new GenericSearchField(Constants.TYPES.Customer, PrincipalModel.NAME);

		assertEquals(unit1, product1.getUnit());

		//test inner join
		GenericQuery query = new GenericQuery(ProductModel._TYPECODE);
		query.addInnerJoin(UnitModel._TYPECODE, GenericCondition.createJoinCondition(productUnitField, unitPkField));
		GenericSearchQuery gsquery = createGSQuery(query, true, german);

		Collection products = genericSearchService.search(gsquery).getResult();
		assertEquals(1, products.size());

		//	test double inner join
		query = new GenericQuery(ProductModel._TYPECODE);
		query.addInnerJoin(UnitModel._TYPECODE, GenericCondition.createJoinCondition(productUnitField, unitPkField));
		query.addInnerJoin(ComposedTypeModel._TYPECODE,
				GenericCondition.createJoinCondition(unitComposedTypeField, composedTypePkField));
		gsquery = createGSQuery(query, true, german);
		products = genericSearchService.search(gsquery).getResult();
		assertEquals(1, products.size());

		//	test outer join
		query = new GenericQuery(ProductModel._TYPECODE);
		query.addOuterJoin(UnitModel._TYPECODE, GenericCondition.createJoinCondition(productUnitField, unitPkField));
		gsquery = createGSQuery(query, true, german);
		products = genericSearchService.search(gsquery).getResult();
		assertEquals(5, products.size());

		//	test outer join with order by condition
		query = new GenericQuery(ProductModel._TYPECODE);
		query.addOuterJoin(UnitModel._TYPECODE, GenericCondition.createJoinCondition(productUnitField, unitPkField));
		query.addOrderBy(new GenericSearchOrderBy(unitCodeField, false));
		gsquery = createGSQuery(query, true, german);
		products = genericSearchService.search(gsquery).getResult();
		assertEquals(5, products.size());

		//	test outer join with inappropriate order by condition
		query = new GenericQuery(ProductModel._TYPECODE);
		query.addOuterJoin(UnitModel._TYPECODE, GenericCondition.createJoinCondition(productUnitField, unitPkField));
		query.addOrderBy(new GenericSearchOrderBy(dumbOrderByField, false));
		gsquery = createGSQuery(query, true, german);
		try
		{
			genericSearchService.search(gsquery).getResult();
			fail("Should have thrown RuntimeException here...");
		}
		catch (final Exception e)
		{
			// fine here
			if (LOG.isDebugEnabled())
			{
				LOG.debug(e.getMessage());
			}
		}
	}

	@Test
	public void testLocalized() throws Exception
	{
		final String qualifier = ProductModel.NAME;

		assertNotNull(product5);
		modelService.setAttributeValue(product1, qualifier, Collections.singletonMap(localeDe, "locvalueA"));
		modelService.setAttributeValue(product1, qualifier, Collections.singletonMap(localeEn, "locvalueA"));
		modelService.setAttributeValue(product2, qualifier, Collections.singletonMap(localeDe, "locvalueB"));
		modelService.setAttributeValue(product2, qualifier, Collections.singletonMap(localeEn, "locvalueB"));
		modelService.setAttributeValue(product3, qualifier, Collections.singletonMap(localeDe, "locvalueA"));
		modelService.setAttributeValue(product3, qualifier, Collections.singletonMap(localeEn, "locvalueB"));
		modelService.setAttributeValue(product4, qualifier, Collections.singletonMap(localeDe, "locvalueB"));
		modelService.setAttributeValue(product4, qualifier, Collections.singletonMap(localeEn, "locvalueA"));
		modelService.save(product1);
		modelService.save(product2);
		modelService.save(product3);
		modelService.save(product4);

		assertNull(modelService.getAttributeValue(product5, qualifier));
		modelService.setAttributeValue(product5, qualifier, Collections.singletonMap(localeEn, "trallala"));
		modelService.save(product5);
		assertNotNull(modelService.getAttributeValue(product5, qualifier));

		final GenericSearchField field = new GenericSearchField(ProductModel._TYPECODE, qualifier);
		field.setFieldTypes(Arrays.asList(new Object[]
				{ GenericSearchFieldType.LOCALIZED, GenericSearchFieldType.PROPERTY }));
		final GenericCondition genericCondition = GenericCondition.createConditionForLiteralComparison(field, Operator.IS_NULL);
		GenericQuery query = new GenericQuery(ProductModel._TYPECODE, genericCondition);
		GenericSearchQuery gsquery = createGSQuery(query, true, german);
		gsquery.setLanguage(german);
		SearchResult searchResultDe = genericSearchService.search(gsquery);
		assertCollectionElements(new ArrayList(searchResultDe.getResult()), product5);

		query = new GenericQuery(ProductModel._TYPECODE);
		query.addCondition(GenericCondition.createConditionForValueComparison(field, Operator.EQUAL, product1.getName(localeDe)));
		gsquery = createGSQuery(query, true, german);
		searchResultDe = genericSearchService.search(gsquery);
		assertCollectionElements(new ArrayList(searchResultDe.getResult()), product1, product3);

		gsquery = createGSQuery(query, true, english);
		final SearchResult searchResultEn = genericSearchService.search(gsquery);
		assertCollectionElements(new ArrayList(searchResultEn.getResult()), product1, product4);
	}

	@Test
	public void testResettableValues() throws Exception
	{
		final GenericSearchField codeField = new GenericSearchField(ProductModel._TYPECODE, ProductModel.CODE);
		final GenericSearchField nameField = new GenericSearchField(ProductModel._TYPECODE, ProductModel.NAME);

		//build query
		final String codeQualifier = "code_qual";
		final String nameQualifier = "name_qual";
		final GenericQuery query = new GenericQuery(ProductModel._TYPECODE);
		query.addCondition(GenericCondition.createConditionForValueComparison(codeField, Operator.EQUAL, product1.getCode(),
				codeQualifier));
		query.addCondition(GenericCondition.createConditionForValueComparison(nameField, Operator.EQUAL,
				product1.getName(localeDe), nameQualifier));
		GenericSearchQuery gsquery = createGSQuery(query, true, german);
		Collection products = genericSearchService.search(gsquery).getResult();
		assertEquals(product1, products.iterator().next());

		assertEquals(2, query.getCondition().getResettableValues().size());
		assertTrue(query.getCondition().getResettableValues().containsKey(codeQualifier));
		assertTrue(query.getCondition().getResettableValues().containsKey(nameQualifier));

		//reset values
		query.getCondition().setResettableValue(codeQualifier, product2.getCode());
		query.getCondition().setResettableValue(nameQualifier, product2.getName(localeDe));
		gsquery = createGSQuery(query, true, german);
		products = genericSearchService.search(gsquery).getResult();
		assertEquals(product2, products.iterator().next());
	}

	@Test
	public void testSubQuery() throws Exception
	{
		final GenericSearchField unitCodeField = new GenericSearchField(UnitModel._TYPECODE, UnitModel.CODE);

		//test exists subquery
		GenericQuery query = new GenericQuery(ProductModel._TYPECODE, GenericCondition.getComparison(ProductModel.CODE,
				Operator.EQUAL,
				product1.getCode()));

		query.addSubQuery(ProductModel.UNIT, Operator.IN, UnitModel._TYPECODE).addCondition(
				GenericCondition.getComparison(unitCodeField, Operator.EQUAL, "unit1"));
		GenericSearchQuery gsquery = createGSQuery(query, true, german);

		Collection products = genericSearchService.search(gsquery).getResult();
		assertEquals(1, products.size());

		//test is_in subquery
		query = new GenericQuery(ProductModel._TYPECODE, GenericCondition.getComparison(ProductModel.CODE, Operator.EQUAL,
				product1.getCode()));

		query.addSubQuery(Operator.EXISTS, UnitModel._TYPECODE).addCondition(
				GenericCondition.getComparison(unitCodeField, Operator.EQUAL, "unit1"));
		gsquery = createGSQuery(query, true, german);
		products = genericSearchService.search(gsquery).getResult();
		assertEquals(1, products.size());

		assertEquals(0, query.getCondition().getResettableValues().size());

		/*
		 * test subtypes upon same type:
		 *
		 * SELECT {PK} FROM {Product} WHERE {code} IN ( {{ SELECT {code} FROM {Product} WHERE {unit}=unit1 }}) -> must
		 * return product1 only
		 */

		query = new GenericQuery(ProductModel._TYPECODE);
		final GenericQuery subquery = query.addSubQuery(ProductModel.CODE, // means outer query type.code !!!
				Operator.IN, ProductModel._TYPECODE // initial type of the subquery
		);
		subquery.addSelectField(new GenericSelectField(ProductModel.CODE)); // means subquery type.code !!!
		subquery.addCondition(GenericCondition.equals(new GenericSearchField(ProductModel.UNIT), unit1));
		products = genericSearchService.search(query).getResult();
		assertEquals(Collections.singletonList(product1), products);

		/*
		 * test self-join
		 *
		 * SELECT {PK}, {other:pk} FROM {Product JOIN Product AS other ON {code} = {other:code} } WHERE {code} IN (
		 * 'product2', 'product4' )
		 */
		query = new GenericQuery(ProductModel._TYPECODE);
		query.addOuterJoin(ProductModel._TYPECODE, "other", GenericCondition.getComparison(
				new GenericSearchField(ProductModel.CODE), Operator.EQUAL, new GenericSearchField("other", ProductModel.CODE)));
		query.addCondition(GenericCondition.getComparison(ProductModel.CODE, Operator.IN,
				Arrays.asList(product2.getCode(), product4.getCode())));
		products = genericSearchService.search(query).getResult();
		assertEquals(new HashSet(Arrays.asList(product2, product4)), new HashSet(products));

		// test duplicate alias error
		query = new GenericQuery(ProductModel._TYPECODE);
		query.addOuterJoin(ProductModel._TYPECODE, // this must throw an error upon flexiblesearch generation
				GenericCondition.getComparison(new GenericSearchField(ProductModel.CODE), Operator.EQUAL,
						new GenericSearchField(
								"other", ProductModel.CODE)));
		try
		{
			query.toFlexibleSearch(new HashMap());
			fail("InvalidParameterException expected");
		}
		catch (final InvalidParameterException e)
		{
			// fine
		}
	}

	@Test
	public void testConditionWithBrackets() throws Exception
	{
		final ProductModel productWithBrackets1 = makeProductWithBrackets("prodWithBrackets1", "test[abc]");
		final ProductModel productWithBrackets2 = makeProductWithBrackets("prodWithBrackets2", "[123]abc");
		final ProductModel productWithBrackets3 = makeProductWithBrackets("prodWithBrackets3", "test[product-123]");

		final GenericSearchField nameField = new GenericSearchField(ProductModel._TYPECODE, ProductModel.NAME);

		//build query
		final String nameQualifier = "name_like";
		final GenericQuery query = new GenericQuery(ProductModel._TYPECODE);

		//1st attempt - search param without brackets
		query.addCondition(GenericCondition.createConditionForValueComparison(nameField, Operator.LIKE,
				"%123%", nameQualifier));
		GenericSearchQuery gsquery = createGSQuery(query, true, english);
		final Collection products = genericSearchService.search(gsquery).getResult();
		assertEquals(2, products.size());
		assertCollectionElements(products, productWithBrackets2, productWithBrackets3);
		verifyTranslatedQueryParam(query, "%123%");

		//2nd attempt - search param in brackets
		final GenericQuery query2 = new GenericQuery(ProductModel._TYPECODE);
		query2.addCondition(GenericCondition.createConditionForValueComparison(nameField, Operator.LIKE,
				"%[abc]%", nameQualifier));
		gsquery = createGSQuery(query2, true, english);
		final Collection productsFoundWithSearchStringInBrackets = genericSearchService.search(gsquery).getResult();

		assertEquals(1, productsFoundWithSearchStringInBrackets.size());
		assertCollectionElements(productsFoundWithSearchStringInBrackets, productWithBrackets1);
		verifyTranslatedQueryParam(query2, Config.isSQLServerUsed() ? "%[[]abc]%" : "%[abc]%");
	}

	@Test
	public void shouldReturnCorrectResultForEqualsSearchQuery()
	{
		final GenericSearchField codeField = new GenericSearchField(ProductModel.CODE);
		for (final String code : getBracketRelatedCodes())
		{
			createAndSaveProduct(code);
			final GenericCondition condition = GenericCondition.createConditionForValueComparison(codeField, Operator.EQUAL,
					code);
			final GenericSearchQuery query = new GenericSearchQuery(new GenericQuery(ProductModel._TYPECODE, condition));
			final List<ProductModel> foundProducts = genericSearchService.<ProductModel>search(query).getResult();
			assertThat(foundProducts).hasSize(1);
			assertThat(foundProducts).extracting(ProductModel::getCode).containsOnly(code);
		}
	}

	@Test
	public void shouldReturnCorrectResultForLikeSearchQuery()
	{
		populateProductsWithBracketRelatedCodes();

		final GenericSearchField codeField = new GenericSearchField(ProductModel.CODE);
		final GenericCondition condition = GenericCondition.createConditionForValueComparison(codeField, Operator.LIKE,
				"[]Test%");
		final GenericSearchQuery query = new GenericSearchQuery(new GenericQuery(ProductModel._TYPECODE, condition));
		final List<ProductModel> foundProducts = genericSearchService.<ProductModel>search(query).getResult();

		assertThat(foundProducts).hasSize(4);
		assertThat(foundProducts).extracting(ProductModel::getCode).containsExactlyInAnyOrder("[]Test", "[]Test%", "[]Test%1", "[]Test1");
	}

	private void populateProductsWithBracketRelatedCodes()
	{
		for (final String code : getBracketRelatedCodes())
		{
			createAndSaveProduct(code);
		}
	}

	private String[] getBracketRelatedCodes()
	{
		return new String[]{ "Test%1", "Test%2", "[Test]", "[Test%]", "Test%", "[Test%1]", "Test [Test]", "Test [", "Test [%", "Test []", "[]Test", "[]Test%", "[]Test%1", "[]Test1", "[][Test]", "[][[Test]]" };
	}

	@Test
	public void shouldReturnCorrectResultForCaseInsensitiveSearchQuery()
	{
		//given
		final ProductModel product = modelService.create(ProductModel.class);
		product.setCode("test");
		product.setCatalogVersion(catalogVersionModel);
		modelService.save(product);
		final GenericSearchField codeField = new GenericSearchField(ProductModel.CODE);

		for (final String conditionValue : getValuesForCaseInsensitiveCondition())
		{
			final GenericCondition condition = GenericCondition.createConditionForValueComparison(codeField, Operator.EQUAL, conditionValue, true);
			final GenericSearchQuery query = new GenericSearchQuery(new GenericQuery(ProductModel._TYPECODE, condition));

			//when
			final List<ProductModel> foundProducts = genericSearchService.<ProductModel>search(query).getResult();

			//then
			assertThat(foundProducts).contains(product);
		}
	}

	@Test
	public void testFilterByClobField()
	{
		//given
		prepareProductWithDescription();

		//when
		final GenericSearchField field = new GenericSearchField(ProductModel._TYPECODE, ProductModel.DESCRIPTION);
		field.setFieldTypes(Arrays.asList(GenericSearchFieldType.LOCALIZED, GenericSearchFieldType.PROPERTY));
		final GenericCondition condition = GenericCondition.getComparison(field, Operator.EQUAL, "locvalueA");
		final GenericQuery query = new GenericQuery(ProductModel._TYPECODE, condition);
		final GenericSearchQuery gsquery = createGSQuery(query, true, german);

		final SearchResult<ProductModel> searchResultDe;

		// then
		searchResultDe = genericSearchService.search(gsquery);
		assertCollectionElements(new ArrayList<ProductModel>(searchResultDe.getResult()), product1, product3);
	}

	@Test
	public void testOrderByClobFieldWithoutLikeFilter()
	{
		//given
		prepareProductWithDescription();

		//when
		final GenericSearchField field = new GenericSearchField(ProductModel._TYPECODE, ProductModel.DESCRIPTION);
		final GenericSearchField fieldName = new GenericSearchField(ProductModel._TYPECODE, ProductModel.NAME);
		field.setFieldTypes(Arrays.asList(GenericSearchFieldType.LOCALIZED, GenericSearchFieldType.PROPERTY));
		final GenericCondition condition;
		GenericQuery query;
		GenericSearchQuery gsquery;

		//when
		condition = GenericCondition.getComparison(fieldName, Operator.EQUAL, NAME_TEST_01);
		query = new GenericQuery(ProductModel._TYPECODE, condition);
		query.addOrderBy(new GenericSearchOrderBy(field, true));
		gsquery = createGSQuery(query, true, english);
		SearchResult<ProductModel> searchResultEn = genericSearchService.search(gsquery);

		//then
		assertCollectionElements(new ArrayList<ProductModel>(searchResultEn.getResult()), product1, product2, product3, product4,
				product5);
		assertThat(searchResultEn.getResult().get(0)).isEqualTo(product1);

		//when
		query = new GenericQuery(ProductModel._TYPECODE);
		query.addOrderBy(new GenericSearchOrderBy(field, false));
		gsquery = createGSQuery(query, true, english);
		searchResultEn = genericSearchService.search(gsquery);

		//then
		assertCollectionElements(new ArrayList<ProductModel>(searchResultEn.getResult()), product1, product2, product3, product4,
				product5);
		assertThat(searchResultEn.getResult().get(0)).isEqualTo(product5);
	}

	@Test
	public void testOrderByClobFieldWithLikeFilter()
	{
		//given
		prepareProductWithDescription();

		//when
		final GenericSearchField field = new GenericSearchField(ProductModel._TYPECODE, ProductModel.DESCRIPTION);
		field.setFieldTypes(Arrays.asList(GenericSearchFieldType.LOCALIZED, GenericSearchFieldType.PROPERTY));
		final GenericCondition condition;
		GenericQuery query;
		GenericSearchQuery gsquery;

		//when
		condition = GenericCondition.getComparison(field, Operator.LIKE, "locvalueA%");

		query = new GenericQuery(ProductModel._TYPECODE, condition);
		query.addOrderBy(new GenericSearchOrderBy(field, true));
		gsquery = createGSQuery(query, true, english);
		SearchResult<ProductModel> searchResultEn = genericSearchService.search(gsquery);

		//then
		assertCollectionElements(new ArrayList<ProductModel>(searchResultEn.getResult()), product1, product4);
		assertThat(searchResultEn.getResult().get(0)).isEqualTo(product1);

		//when
		query = new GenericQuery(ProductModel._TYPECODE, condition);
		query.addOrderBy(new GenericSearchOrderBy(field, false));
		gsquery = createGSQuery(query, true, english);
		searchResultEn = genericSearchService.search(gsquery);

		//then
		assertCollectionElements(new ArrayList<ProductModel>(searchResultEn.getResult()), product1, product4);
		assertThat(searchResultEn.getResult().get(0)).isEqualTo(product4);
	}

	protected void prepareProductWithDescription()
	{
		final String qualifier = ProductModel.DESCRIPTION;

		product1.setName(NAME_TEST_01);
		product2.setName(NAME_TEST_01);
		product3.setName(NAME_TEST_01);
		product4.setName(NAME_TEST_01);
		product5.setName(NAME_TEST_01);

		modelService.setAttributeValue(product1, qualifier, Collections.singletonMap(localeDe, "locvalueA"));
		modelService.setAttributeValue(product1, qualifier, Collections.singletonMap(localeEn, "locvalueAa"));
		modelService.setAttributeValue(product2, qualifier, Collections.singletonMap(localeDe, "locvalueB"));
		modelService.setAttributeValue(product2, qualifier, Collections.singletonMap(localeEn, "locvalueBa"));
		modelService.setAttributeValue(product3, qualifier, Collections.singletonMap(localeDe, "locvalueA"));
		modelService.setAttributeValue(product3, qualifier, Collections.singletonMap(localeEn, "locvalueBb"));
		modelService.setAttributeValue(product4, qualifier, Collections.singletonMap(localeDe, "locvalueB"));
		modelService.setAttributeValue(product4, qualifier, Collections.singletonMap(localeEn, "locvalueAb"));
		modelService.setAttributeValue(product5, qualifier, Collections.singletonMap(localeDe, "locvalueB"));
		modelService.setAttributeValue(product5, qualifier, Collections.singletonMap(localeEn, "locvalueBc"));
		modelService.saveAll(product1, product2, product3, product4, product5);
	}

	private String[] getValuesForCaseInsensitiveCondition()
	{
		return new String[]
				{
						"test",
						"Test",
						"TEST",
						"TeSt"
				};
	}

	private void verifyTranslatedQueryParam(final GenericQuery genericQuery, final String expectedValue)
	{
		final Map<String, Object> values = new HashMap();
		genericQuery.toFlexibleSearch(values);
		assertEquals(1, values.size());

		assertTrue(values.containsValue(expectedValue));
	}


	private GenericSearchQuery createGSQuery(final GenericQuery query, final boolean dontNeedTotal, final LanguageModel language)
	{
		final GenericSearchQuery gsquery = new GenericSearchQuery(query);
		gsquery.setDontNeedTotal(dontNeedTotal);
		gsquery.setLanguage(language);
		return gsquery;
	}

	private ProductModel createAndSaveProduct(final String code)
	{
		final ProductModel product = modelService.create(ProductModel.class);
		product.setCode(code);
		product.setCatalogVersion(catalogVersionModel);
		modelService.save(product);
		return product;
	}

	//--
	private final ProductModel makeProduct(final int number) throws Exception
	{
		final ProductModel result = modelService.create(ProductModel.class);
		modelService.initDefaults(result);
		result.setCode("product" + number);
		result.setName(result.getCode(), localeDe);
		result.setName(result.getCode(), localeEn);
		result.setCatalogVersion(catalogVersionModel);
		return result;
	}

	private final ProductModel makeProductWithBrackets(final String code, final String name) throws Exception
	{
		final ProductModel result = modelService.create(ProductModel.class);
		result.setCode(code);
		result.setName(name);
		result.setCatalogVersion(catalogVersionModel);
		modelService.save(result);
		return result;
	}


	private final UnitModel makeUnit(final String type, final String code) throws Exception
	{
		final UnitModel result = modelService.create(UnitModel.class);
		modelService.initDefaults(result);
		result.setCode(code);
		result.setUnitType(type);
		return result;
	}
}
