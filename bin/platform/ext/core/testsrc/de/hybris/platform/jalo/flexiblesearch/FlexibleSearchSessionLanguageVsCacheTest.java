/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jalo.flexiblesearch;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.jalo.SearchResult;
import de.hybris.platform.jalo.SessionContext;
import de.hybris.platform.jalo.c2l.Language;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.util.SQLSearchResult;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class FlexibleSearchSessionLanguageVsCacheTest extends ServicelayerBaseTest
{

	private Language de, en;

	private SessionContext deCtx, enCtx;

	private FlexibleSearch flexibleSearch;
	private String uniqueString;

	@Before
	public void setUp() throws Exception
	{
		flexibleSearch = FlexibleSearch.getInstance();
		uniqueString = UUID.randomUUID().toString();

		de = getOrCreateLanguage("de");
		de.setActive(false);
		en = getOrCreateLanguage("en");
		en.setActive(true);

		deCtx = jaloSession.createSessionContext();
		deCtx.setLanguage(de);
		enCtx = jaloSession.createSessionContext();
		enCtx.setLanguage(en);

		de.setName(deCtx, "Deutsch");
		de.setName(enCtx, "German");

		en.setName(deCtx, "Englisch");
		en.setName(enCtx, "English");
	}

	@Test
	public void testSessionLanguageDependentQuery() throws FlexibleSearchException
	{
		// first query: session language == DE
		final SearchResult<Language> resDe = executeQueryOnLocalizedField(deCtx);
		final List<Language> itemsDe = assertSearchResultIsNotFromCache(resDe);

		// second query: session language == EN - should not be from cache
		final SearchResult<Language> resEn = executeQueryOnLocalizedField(enCtx);
		final List<Language> itemsEn = assertSearchResultIsNotFromCache(resEn);

		assertThat(itemsEn).containsExactlyElementsOf(itemsDe);
	}

	@Test
	public void testFixedLanguageQuery() throws FlexibleSearchException
	{
		// first query: session language == DE
		final SearchResult<Language> resDe = executeQueryOnLocalizedField(deCtx, "de");
		assertSearchResultIsNotFromCache(resDe);
		final List<Language> itemsDe = resDe.getResult();

		// second query: session language == EN - should be from cache
		final SearchResult<Language> resEn = executeQueryOnLocalizedField(enCtx, "de");
		final List<Language> itemsEn = assertSearchResultIsFromCache(resEn);
		assertThat(itemsEn).containsExactlyElementsOf(itemsDe);
	}

	@Test
	public void testSessionLanguageIndependentQuery() throws FlexibleSearchException
	{
		// first query: session language == DE
		final SearchResult<Language> res = executeQueryOnNonLocalizedField(deCtx);
		final List<Language> itemsDe = assertSearchResultIsNotFromCache(res);

		// second query: session language == EN - should be from cache
		final SearchResult<Language> res2 = executeQueryOnNonLocalizedField(enCtx);
		final List<Language> itemsEn = assertSearchResultIsFromCache(res2);
		assertThat(itemsEn).containsExactlyElementsOf(itemsDe);
	}

	private List<Language> assertSearchResultIsFromCache(final SearchResult<Language> resEn)
	{
		return verifySearchResult(resEn, true);
	}

	private List<Language> assertSearchResultIsNotFromCache(final SearchResult<Language> resEn)
	{
		return verifySearchResult(resEn, false);
	}

	private List<Language> verifySearchResult(final SearchResult<Language> res, final boolean expected)
	{
		assertThat(res).isNotNull().isInstanceOf(SQLSearchResult.class);
		final List<Language> itemsEn = res.getResult();
		assertThat(itemsEn).contains(de, en);

		assertThat(((SQLSearchResult<?>) res).isFromCache()).isEqualTo(expected);
		return itemsEn;
	}

	private SearchResult<Language> executeQueryOnLocalizedField(final SessionContext ctx)
	{
		return executeQueryOnLocalizedField(ctx, null);
	}

	private SearchResult<Language> executeQueryOnLocalizedField(final SessionContext ctx, final String language)
	{
		final Map<String, Object> params = new HashMap<>();
		params.put("uniqueString", uniqueString);

		final String langCode = language != null ? String.format("[%s]", language) : "";
		final String query = String.format("SELECT {PK} FROM {Language} WHERE {name%s} <> ?uniqueString", langCode);
		return flexibleSearch.search(ctx, query, params, Language.class);
	}

	private SearchResult<Language> executeQueryOnNonLocalizedField(final SessionContext ctx)
	{
		final Map<String, Object> params = new HashMap<>();
		params.put("uniqueString", uniqueString);
		return flexibleSearch.search(ctx, "SELECT {PK} FROM {Language} WHERE {isocode} <> ?uniqueString", params, Language.class);
	}
}
