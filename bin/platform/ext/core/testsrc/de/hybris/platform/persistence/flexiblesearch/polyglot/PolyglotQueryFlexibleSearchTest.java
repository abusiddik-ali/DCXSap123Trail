/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.flexiblesearch.polyglot;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.GenericCondition;
import de.hybris.platform.core.GenericConditionList;
import de.hybris.platform.core.GenericQuery;
import de.hybris.platform.core.GenericSearchField;
import de.hybris.platform.core.GenericSelectField;
import de.hybris.platform.core.Operator;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.jalo.ConsistencyCheckException;
import de.hybris.platform.jalo.product.Product;
import de.hybris.platform.jalo.user.Title;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.Resource;

import org.junit.Ignore;
import org.junit.Test;


@IntegrationTest
public class PolyglotQueryFlexibleSearchTest extends ServicelayerTransactionalBaseTest
{
	@Resource
	private ModelService modelService;
	@Resource
	private FlexibleSearchService flexibleSearchService;

	@Test
	public void testPolyglotQuery()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");

		modelService.saveAll(title1);

		// when
		final String query = "GET {Title} WHERE {code}=?code";
		final Map<String, Object> params = new HashMap<>();
		params.put("code", "firstTitle");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		try
		{
			assertEquals(1, result.size());
			assertEquals("firstTitle", ((TitleModel) result.get(0)).getCode());
		}
		finally
		{
			modelService.remove(title1);
		}
	}

	@Test
	public void testPolyglotQueryWithBraces()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");

		modelService.saveAll(title1);

		// when
		final String query = "GET {Title} WHERE ({code}=?code)";
		final Map<String, Object> params = new HashMap<>();
		params.put("code", "firstTitle");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		assertEquals(1, result.size());
		assertEquals("firstTitle", ((TitleModel) result.get(0)).getCode());

	}

	@Test
	public void testPolyglotQueryForAllTitles()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");

		final TitleModel title2 = new TitleModel();
		title2.setCode("secondTitle");

		modelService.saveAll(title1, title2);

		// when
		final String query = "GET {Title}";
		final List result = flexibleSearchService.search(query).getResult();

		// then

		assertEquals(2, result.size());
		assertEquals("firstTitle", ((TitleModel) result.get(0)).getCode());
		assertEquals("secondTitle", ((TitleModel) result.get(1)).getCode());
	}


	@Test
	public void testPolyglotQueryForAllTitlesWithOrderBy()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("b");

		final TitleModel title2 = new TitleModel();
		title2.setCode("a");

		modelService.saveAll(title1, title2);

		// when
		final String query = "GET {Title} ORDER by {code}";
		final List result = flexibleSearchService.search(query).getResult();

		// then
		assertEquals(2, result.size());
		assertEquals("a", ((TitleModel) result.get(0)).getCode());
		assertEquals("b", ((TitleModel) result.get(1)).getCode());
	}

	@Test
	public void testPolyglotQueryWithMultipleBraces()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");

		modelService.saveAll(title1);

		// when
		final String query = "GET {Title} WHERE (((({code}=?code))))";
		final Map<String, Object> params = new HashMap<>();
		params.put("code", "firstTitle");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		try
		{
			assertEquals(1, result.size());
			assertEquals("firstTitle", ((TitleModel) result.get(0)).getCode());
		}
		finally
		{
			modelService.remove(title1);
		}
	}

	@Test
	//@Ignore
	public void testPolyglotQueryWithAnd()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");
		title1.setName("testName", getOrCreateLanguage("en").getLocale());

		final TitleModel title2 = new TitleModel();
		title2.setCode("secondTitle");
		title2.setName("testName", getOrCreateLanguage("en").getLocale());
		modelService.saveAll(title1, title2);

		// when
		final String query = "GET {Title} WHERE {name[en]}=?name AND {code}=?code";
		final Map<String, Object> params = new HashMap<>();
		params.put("code", "firstTitle");
		params.put("name", "testName");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		try
		{
			assertEquals(1, result.size());
			assertEquals("firstTitle", ((TitleModel) result.get(0)).getCode());
			assertEquals("testName", ((TitleModel) result.get(0)).getName());
		}
		finally
		{
			modelService.removeAll(title1, title2);
		}
	}

	@Test
	//@Ignore
	public void testPolyglotQueryWithAndAndBraces()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");
		title1.setName("testName", getOrCreateLanguage("en").getLocale());

		final TitleModel title2 = new TitleModel();
		title2.setCode("secondTitle");
		title2.setName("testName", getOrCreateLanguage("en").getLocale());
		modelService.saveAll(title1, title2);

		// when
		final String query = "GET {Title} WHERE (({name[en]}=?name) AND {code}=?code)";
		final Map<String, Object> params = new HashMap<>();
		params.put("code", "firstTitle");
		params.put("name", "testName");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		try
		{
			assertEquals(1, result.size());
			assertEquals("firstTitle", ((TitleModel) result.get(0)).getCode());
			assertEquals("testName", ((TitleModel) result.get(0)).getName());
		}
		finally
		{
			modelService.removeAll(title1, title2);
		}
	}

	@Test
	//@Ignore
	public void testPolyglotQueryWithOr()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");
		title1.setName("testName", getOrCreateLanguage("en").getLocale());

		final TitleModel title2 = new TitleModel();
		title2.setCode("secondTitle");
		title2.setName("testName", getOrCreateLanguage("en").getLocale());
		modelService.saveAll(title1, title2);

		// when
		final String query = "GET {Title} WHERE {code}=?code1 OR {code}=?code2 ORDER BY {code}";
		final Map<String, Object> params = new HashMap<>();
		params.put("code1", "firstTitle");
		params.put("code2", "secondTitle");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		try
		{
			assertEquals(2, result.size());
			assertEquals("firstTitle", ((TitleModel) result.get(0)).getCode());
			assertEquals("testName", ((TitleModel) result.get(0)).getName());
			assertEquals("secondTitle", ((TitleModel) result.get(1)).getCode());
			assertEquals("testName", ((TitleModel) result.get(1)).getName());
		}
		finally
		{
			modelService.removeAll(title1, title2);
		}
	}

	@Test
	//@Ignore
	public void testPolyglotQueryWithAndAndOr()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");
		title1.setName("testName", getOrCreateLanguage("en").getLocale());

		final TitleModel title2 = new TitleModel();
		title2.setCode("secondTitle");
		title2.setName("testName2", getOrCreateLanguage("en").getLocale());
		modelService.saveAll(title1, title2);

		// when
		final String query = "GET {Title} WHERE {code}=?code1 OR {code}=?code2 AND {name[en]}=?name";
		final Map<String, Object> params = new HashMap<>();
		params.put("code1", "firstTitle");
		params.put("code2", "secondTitle");
		params.put("name", "testName2");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		try
		{
			assertEquals(2, result.size());
			assertEquals("firstTitle", ((TitleModel) result.get(0)).getCode());
			assertEquals("testName", ((TitleModel) result.get(0)).getName());
			assertEquals("secondTitle", ((TitleModel) result.get(1)).getCode());
			assertEquals("testName2", ((TitleModel) result.get(1)).getName());
		}
		finally
		{
			modelService.removeAll(title1, title2);
		}
	}

	@Test
	//@Ignore
	public void testPolyglotQueryWithAndAndOrInBraces()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");
		title1.setName("testName", getOrCreateLanguage("en").getLocale());

		final TitleModel title2 = new TitleModel();
		title2.setCode("secondTitle");
		title2.setName("testName2", getOrCreateLanguage("en").getLocale());
		modelService.saveAll(title1, title2);

		// when
		final String query = "GET {Title} WHERE ({code}=?code1 OR {code}=?code2) AND {name[en]}=?name";
		final Map<String, Object> params = new HashMap<>();
		params.put("code1", "firstTitle");
		params.put("code2", "secondTitle");
		params.put("name", "testName2");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		try
		{
			assertEquals(1, result.size());
			assertEquals("secondTitle", ((TitleModel) result.get(0)).getCode());
			assertEquals("testName2", ((TitleModel) result.get(0)).getName());
		}
		finally
		{
			modelService.removeAll(title1, title2);
		}
	}

	@Test
	//@Ignore
	public void testPolyglotQueryWithOrderBy()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");
		title1.setName("testName", getOrCreateLanguage("en").getLocale());

		final TitleModel title2 = new TitleModel();
		title2.setCode("secondTitle");
		title2.setName("testName", getOrCreateLanguage("en").getLocale());
		modelService.saveAll(title1, title2);

		final TitleModel title3 = new TitleModel();
		title3.setCode("thirdTitle");
		title3.setName("testName", getOrCreateLanguage("en").getLocale());
		modelService.saveAll(title1, title2, title3);

		// when
		final String query = "GET {Title} WHERE {name[en]}=?name ORDER BY {code}";
		final Map<String, Object> params = new HashMap<>();
		params.put("name", "testName");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		try
		{
			assertEquals(3, result.size());
			assertEquals("firstTitle", ((TitleModel) result.get(0)).getCode());
			assertEquals("testName", ((TitleModel) result.get(0)).getName());
			assertEquals("secondTitle", ((TitleModel) result.get(1)).getCode());
			assertEquals("testName", ((TitleModel) result.get(1)).getName());
			assertEquals("thirdTitle", ((TitleModel) result.get(2)).getCode());
			assertEquals("testName", ((TitleModel) result.get(2)).getName());
		}
		finally
		{
			modelService.removeAll(title1, title2, title3);
		}
	}

	@Test
	//@Ignore
	public void testPolyglotQueryWithOrderByAsc()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");
		title1.setName("testName", getOrCreateLanguage("en").getLocale());

		final TitleModel title2 = new TitleModel();
		title2.setCode("secondTitle");
		title2.setName("testName", getOrCreateLanguage("en").getLocale());

		final TitleModel title3 = new TitleModel();
		title3.setCode("thirdTitle");
		title3.setName("testName", getOrCreateLanguage("en").getLocale());
		modelService.saveAll(title1, title2, title3);

		// when
		final String query = "GET {Title} WHERE {name[en]}=?name ORDER BY {code} ASC";
		final Map<String, Object> params = new HashMap<>();
		params.put("name", "testName");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		try
		{
			assertEquals(3, result.size());
			assertEquals("firstTitle", ((TitleModel) result.get(0)).getCode());
			assertEquals("testName", ((TitleModel) result.get(0)).getName());
			assertEquals("secondTitle", ((TitleModel) result.get(1)).getCode());
			assertEquals("testName", ((TitleModel) result.get(1)).getName());
			assertEquals("thirdTitle", ((TitleModel) result.get(2)).getCode());
			assertEquals("testName", ((TitleModel) result.get(2)).getName());
		}
		finally
		{
			modelService.removeAll(title1, title2, title3);
		}
	}

	@Test
	//@Ignore
	public void testPolyglotQueryWithOrderByDesc()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");
		title1.setName("testName", getOrCreateLanguage("en").getLocale());

		final TitleModel title2 = new TitleModel();
		title2.setCode("secondTitle");
		title2.setName("testName", getOrCreateLanguage("en").getLocale());

		final TitleModel title3 = new TitleModel();
		title3.setCode("thirdTitle");
		title3.setName("testName", getOrCreateLanguage("en").getLocale());

		modelService.saveAll(title1, title2, title3);

		// when
		final String query = "GET {Title} WHERE {name[en]}=?name ORDER BY {code} DESC";
		final Map<String, Object> params = new HashMap<>();
		params.put("name", "testName");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		try
		{
			assertEquals(3, result.size());
			assertEquals("thirdTitle", ((TitleModel) result.get(0)).getCode());
			assertEquals("testName", ((TitleModel) result.get(0)).getName());
			assertEquals("secondTitle", ((TitleModel) result.get(1)).getCode());
			assertEquals("testName", ((TitleModel) result.get(1)).getName());
			assertEquals("firstTitle", ((TitleModel) result.get(2)).getCode());
			assertEquals("testName", ((TitleModel) result.get(2)).getName());
		}
		finally
		{
			modelService.removeAll(title1, title2, title3);
		}
	}

	@Test
	//@Ignore
	public void testPolyglotQueryWithOrderByMultipleColumns()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");
		title1.setName("testName", getOrCreateLanguage("en").getLocale());
		title1.setName("aaa", getOrCreateLanguage("de").getLocale());


		final TitleModel title2 = new TitleModel();
		title2.setCode("secondTitle");
		title2.setName("testName", getOrCreateLanguage("en").getLocale());
		title2.setName("ccc", getOrCreateLanguage("de").getLocale());
		modelService.saveAll(title1, title2);

		final TitleModel title3 = new TitleModel();
		title3.setCode("thirdTitle");
		title3.setName("testName", getOrCreateLanguage("en").getLocale());
		title3.setName("bbb", getOrCreateLanguage("de").getLocale());
		modelService.saveAll(title1, title2, title3);

		// when
		final String query = "GET {Title} WHERE {name[en]}=?name ORDER BY {name[de]}, {code} ASC";
		final Map<String, Object> params = new HashMap<>();
		params.put("name", "testName");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		try
		{
			assertEquals(3, result.size());
			assertEquals("firstTitle", ((TitleModel) result.get(0)).getCode());
			assertEquals("testName", ((TitleModel) result.get(0)).getName());
			assertEquals("aaa", ((TitleModel) result.get(0)).getName(getOrCreateLanguage("de").getLocale()));
			assertEquals("thirdTitle", ((TitleModel) result.get(1)).getCode());
			assertEquals("testName", ((TitleModel) result.get(1)).getName());
			assertEquals("bbb", ((TitleModel) result.get(1)).getName(getOrCreateLanguage("de").getLocale()));
			assertEquals("secondTitle", ((TitleModel) result.get(2)).getCode());
			assertEquals("testName", ((TitleModel) result.get(2)).getName());
			assertEquals("ccc", ((TitleModel) result.get(2)).getName(getOrCreateLanguage("de").getLocale()));
		}
		finally
		{
			modelService.removeAll(title1, title2, title3);
		}
	}

	@Test
	@Ignore
	public void testTitleConsistencyCheck()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("testTitle");

		final TitleModel title2 = new TitleModel();
		title2.setCode("testTitle");
		title2.setName("testName", getOrCreateLanguage("en").getLocale());

		modelService.save(title1);

		// when
		boolean exception = false;
		try
		{
			modelService.save(title2);
		}
		catch (final ModelSavingException mse)
		{
			if (mse.getCause() instanceof ConsistencyCheckException)
			{
				exception = true;
			}
		}

		// then
		try
		{
			assertEquals("ConsistencyCheckException should be thrown", true, exception);
		}
		finally
		{
			modelService.removeAll(title1, title2);
		}
	}

	@Test
	@Ignore
	public void testPolyglotQueryWithEverything()
	{
		// when
		final String query = "GET {Title} WHERE ({code}=?code1 OR {code}=?code2) AND {name[en]}=?name ORDER BY {code} DESC";
	}

	@Test
	public void testNotEquals()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");
		final TitleModel title2 = new TitleModel();
		title2.setCode("secondTitle");

		modelService.saveAll(title1, title2);

		// when
		final String query = "GET {Title} WHERE {code}<>?code ORDER BY {code}";
		final Map<String, Object> params = new HashMap<>();
		params.put("code", "firstTitle");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		try
		{
			assertEquals(1, result.size());
			assertEquals("secondTitle", ((TitleModel) result.get(0)).getCode());
		}
		finally
		{
			modelService.removeAll(title1, title2);
		}
	}

	@Test
	public void testPolyglotQueryWithIsNull()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");

		modelService.saveAll(title1);

		// when
		final String query = "GET {Title} WHERE {code}=?code and {owner} IS NULL ";
		final Map<String, Object> params = new HashMap<>();
		params.put("code", "firstTitle");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		try
		{
			assertEquals(1, result.size());
			assertEquals("firstTitle", ((TitleModel) result.get(0)).getCode());
		}
		finally
		{
			modelService.remove(title1);
		}
	}

	@Test
	public void testPolyglotQueryWithIsNotNull()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");
		title1.setName("testName", getOrCreateLanguage("en").getLocale());

		modelService.saveAll(title1);

		// when
		final String query = "GET {Title} WHERE {code}=?code and {name[en]} IS NOT NULL ";
		final Map<String, Object> params = new HashMap<>();
		params.put("code", "firstTitle");
		final List result = flexibleSearchService.search(query, params).getResult();

		// then
		try
		{
			assertEquals(1, result.size());
			assertEquals("firstTitle", ((TitleModel) result.get(0)).getCode());
		}
		finally
		{
			modelService.remove(title1);
		}
	}

	@Test
	public void testGenericSearchWithMultipleSelectFields()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");
		title1.setName("testName", getOrCreateLanguage("en").getLocale());

		modelService.save(title1);

		final GenericSearchField codeField = new GenericSearchField("Title", Product.CODE);
		final GenericSearchField nameField = new GenericSearchField("Title", Product.NAME);
		final GenericSelectField codeSelectField = new GenericSelectField("Title", Product.CODE, String.class);
		final GenericSelectField nameSelectField = new GenericSelectField("Title", Product.NAME, String.class);
		final GenericCondition condition = GenericCondition.createConditionForValueComparison(codeField, Operator.EQUAL,
				"firstTitle");
		final GenericConditionList conditionList = GenericCondition.createConditionList(condition);
		conditionList.addToConditionList(GenericCondition.createIsNotNullCondition(nameField));
		final GenericQuery query = new GenericQuery("Title");
		query.addCondition(conditionList);
		query.addSelectField(codeSelectField);
		query.addSelectField(nameSelectField);

		// when
		final List result = jaloSession.search(query).getResult();

		// then
		try
		{
			assertEquals(1, result.size());
			assertEquals("firstTitle", ((List) result.get(0)).get(0));
			assertEquals("testName", ((List) result.get(0)).get(1));
		}
		finally
		{
			modelService.removeAll(title1);
		}
	}

	@Test
	public void testGenericSearchWithoutSelectFields()
	{
		// given
		final TitleModel title1 = new TitleModel();
		title1.setCode("firstTitle");
		title1.setName("testName", getOrCreateLanguage("en").getLocale());

		modelService.save(title1);

		final GenericQuery query = new GenericQuery("Title");

		// when
		final List result = jaloSession.search(query).getResult();

		// then
		try
		{
			assertThat(result.size()).isGreaterThanOrEqualTo(1);
			assertThat(result).contains((Title) modelService.getSource(title1));
		}
		finally
		{
			modelService.removeAll(title1);
		}
	}
}
