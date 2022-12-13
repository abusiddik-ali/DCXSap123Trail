/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.flexiblesearch.polyglot;

import static de.hybris.platform.persistence.flexiblesearch.polyglot.PolyglotPersistenceFlexibleSearchSupport.tryToConvertToPolyglotCriteria;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.jalo.flexiblesearch.QueryOptions;
import de.hybris.platform.persistence.polyglot.TypeInfoFactory;
import de.hybris.platform.persistence.polyglot.config.TypeInfo;
import de.hybris.platform.persistence.polyglot.search.criteria.Criteria;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import javax.annotation.Resource;

import org.junit.Test;


@IntegrationTest
public class PolyglotQueryCriteriaConversionTest extends ServicelayerBaseTest
{
	@Resource
	private ModelService modelService;
	@Test
	public void testPolyglotQuery()
	{
		// given
		final String query = "GET {Title} WHERE {code}=?code";
		final Map<String, Object> params = new HashMap<>();
		params.put("code", "testTitle");
		final QueryOptions queryOptions = QueryOptions.newBuilder().withQuery(query).withValues(params).build();
		final Optional<Criteria> polyglotQueryCriteriaOptional = tryToConvertToPolyglotCriteria(queryOptions);

		// when
		final Criteria polyglotQueryCriteria = polyglotQueryCriteriaOptional.get();
		final String result = convert(polyglotQueryCriteria);

		// then
		assertEquals("SELECT {PK} FROM {Title} WHERE {code}=?code", result);
	}

	@Test
	public void testPolyglotQueryWithAndAndOrAndBracesWithOrderByDescending()
	{
		// given
		final String query = "GET {Title} WHERE ({name[en]}=?name1 OR {name[en]}=?name2) AND {code}=?code ORDER BY {code} DESC";
		final Map<String, Object> params = new HashMap<>();
		params.put("name1", "testName1");
		params.put("name2", "testName2");
		params.put("code", "testTitle");
		final QueryOptions queryOptions = QueryOptions.newBuilder().withQuery(query).withValues(params).build();
		final Optional<Criteria> polyglotQueryCriteriaOptional = tryToConvertToPolyglotCriteria(queryOptions);

		// when
		final Criteria polyglotQueryCriteria = polyglotQueryCriteriaOptional.get();
		final String result = convert(polyglotQueryCriteria);

		// then
		assertEquals(
				"SELECT {PK} FROM {Title} WHERE (({name[en]}=?name1 or {name[en]}=?name2) and {code}=?code) ORDER BY {code} DESC",
				result);
	}

	@Test
	//@Ignore
	public void testPolyglotQueryWithOrderByMultipleColumns()
	{
		// given
		final String query = "GET {Title} WHERE {name[en]}=?name ORDER BY {name[de]}, {code} DESC";
		final Map<String, Object> params = new HashMap<>();
		params.put("name", "testName");
		final QueryOptions queryOptions = QueryOptions.newBuilder().withQuery(query).withValues(params).build();
		final Optional<Criteria> polyglotQueryCriteriaOptional = tryToConvertToPolyglotCriteria(queryOptions);

		// when
		final Criteria polyglotQueryCriteria = polyglotQueryCriteriaOptional.get();
		final String result = convert(polyglotQueryCriteria);

		// then
		assertEquals("SELECT {PK} FROM {Title} WHERE {name[en]}=?name ORDER BY {name[de]} ASC, {code} DESC", result);

	}

	@Test
	public void testPolyglotQueryWithOrderByMultipleColumnsWithDirections()
	{
		final String query = "GET {JobSearchRestriction} WHERE {job}=?key ORDER BY {jobPOS} ASC,{creationtime} ASC";
		final Map<String, Object> params = new HashMap<>();
		params.put("key", "testKey");
		final QueryOptions queryOptions = QueryOptions.newBuilder().withQuery(query).withValues(params).build();
		final Optional<Criteria> polyglotQueryCriteriaOptional = tryToConvertToPolyglotCriteria(queryOptions);

		// when
		final Criteria polyglotQueryCriteria = polyglotQueryCriteriaOptional.get();
		final String result = convert(polyglotQueryCriteria);

		// then
		assertEquals("SELECT {PK} FROM {JobSearchRestriction} WHERE {job}=?key ORDER BY {jobpos} ASC, {creationtime} ASC",
				result);
	}

	@Test
	public void testPolyglotQueryWithOrderByWithTrailingSpace()
	{
		final String query = "GET {Document} WHERE {sourceItem}=?key ORDER BY {creationtime} ASC ";
		final Map<String, Object> params = new HashMap<>();
		params.put("key", "testKey");
		final QueryOptions queryOptions = QueryOptions.newBuilder().withQuery(query).withValues(params).build();
		final Optional<Criteria> polyglotQueryCriteriaOptional = tryToConvertToPolyglotCriteria(queryOptions);

		// when
		final Criteria polyglotQueryCriteria = polyglotQueryCriteriaOptional.get();
		final String result = convert(polyglotQueryCriteria);

		// then
		assertEquals("SELECT {PK} FROM {Document} WHERE {sourceitem}=?key ORDER BY {creationtime} ASC", result);
	}

	@Test
	public void testPolyglotQueryWithIsNull()
	{
		// given
		final String query = "GET {Title} WHERE {code} IS NULL";

		final QueryOptions queryOptions = QueryOptions.newBuilder().withQuery(query).build();
		final Optional<Criteria> polyglotQueryCriteriaOptional = tryToConvertToPolyglotCriteria(queryOptions);

		// when
		final Criteria polyglotQueryCriteria = polyglotQueryCriteriaOptional.get();
		final String result = convert(polyglotQueryCriteria);

		// then
		assertEquals("SELECT {PK} FROM {Title} WHERE {code} IS NULL", result);
	}

	@Test
	public void testPolyglotQueryWithIsNotNull()
	{
		// given
		final String query = "GET {Title} WHERE {code} IS NOT NULL";

		final QueryOptions queryOptions = QueryOptions.newBuilder().withQuery(query).build();
		final Optional<Criteria> polyglotQueryCriteriaOptional = tryToConvertToPolyglotCriteria(queryOptions);

		// when
		final Criteria polyglotQueryCriteria = polyglotQueryCriteriaOptional.get();
		final String result = convert(polyglotQueryCriteria);

		// then
		assertEquals("SELECT {PK} FROM {Title} WHERE {code} IS NOT NULL", result);
	}

	@Test
	public void testPolyglotQueryWithItemAsParam()
	{
		final TitleModel title = modelService.create(TitleModel.class);
		title.setCode(UUID.randomUUID().toString());
		modelService.saveAll();

		// given
		final String query = "GET {CartEntry} WHERE {order}=?item";

		final QueryOptions queryOptions = QueryOptions.newBuilder()
		                                              .withQuery(query)
		                                              .withValues(Map.of("item", modelService.getSource(title)))
		                                              .build();
		final Optional<Criteria> polyglotQueryCriteriaOptional = tryToConvertToPolyglotCriteria(queryOptions);

		// when
		final Criteria polyglotQueryCriteria = polyglotQueryCriteriaOptional.get();

		final String result = convert(polyglotQueryCriteria);

		// then
		assertThat(result).isEqualTo("SELECT {PK} FROM {CartEntry} WHERE {order}=?item");

		final TypeInfo typeInfo = TypeInfoFactory.getTypeInfo(polyglotQueryCriteria);
		assertThat(typeInfo.getMoreSpecificTypeInfo("order")).isNotNull();
		assertThat(typeInfo.getMoreSpecificTypeInfo("oRdEr")).isNotNull();
	}

	private String convert(final Criteria polyglotQueryCriteria)
	{
		return PolyglotPersistenceFlexibleSearchSupport.convertToFlexibleSearchQueryString(polyglotQueryCriteria);
	}
}
