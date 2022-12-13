/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog;

import static org.assertj.core.api.Assertions.assertThatThrownBy;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.catalog.daos.KeywordDao;
import de.hybris.platform.catalog.impl.DefaultKeywordService;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.catalog.model.KeywordModel;
import de.hybris.platform.servicelayer.exceptions.AmbiguousIdentifierException;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;

import java.util.Arrays;
import java.util.Collections;

import org.assertj.core.api.Assertions;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;


/**
 * tests {@link DefaultKeywordService}
 */
@UnitTest
public class KeywordServiceTest
{
	String keyword = "keyword";
	String typecode = "Typecode";
	CatalogVersionModel catalogVersion = new CatalogVersionModel();
	private DefaultKeywordService keywordService;
	@Mock
	private KeywordDao keywordDao;

	@Before
	public void setUp()
	{
		MockitoAnnotations.initMocks(this);
		keywordService = new DefaultKeywordService();
		keywordService.setKeywordDao(keywordDao);


	}

	@Test
	public void testGetKeyward()
	{
		final KeywordModel keywordModel = new KeywordModel();
		Mockito.when(keywordDao.getKeywords(catalogVersion, keyword)).thenReturn(Collections.singletonList(keywordModel));

		Assertions.assertThat(keywordService.getKeyword(catalogVersion, keyword)).isSameAs(keywordModel);
	}


	@Test
	public void testGetKeywardFailToMany()
	{

		Mockito.when(keywordDao.getKeywords(catalogVersion, keyword)).thenReturn(
				Arrays.asList(new KeywordModel(), new KeywordModel()));

		assertThatThrownBy(() -> keywordService.getKeyword(catalogVersion, keyword))
				.isInstanceOf(AmbiguousIdentifierException.class);
	}

	@Test
	public void testGetKeywardFailNullArg()
	{
		assertThatThrownBy(() -> keywordService.getKeyword(null, keyword)).isInstanceOf(IllegalArgumentException.class);
		assertThatThrownBy(() -> keywordService.getKeyword(catalogVersion, null)).isInstanceOf(IllegalArgumentException.class);
	}

	@Test
	public void testGetKeywardFailEmpty()
	{

		Mockito.when(keywordDao.getKeywords(catalogVersion, keyword)).thenReturn(Collections.emptyList());

		assertThatThrownBy(() -> keywordService.getKeyword(catalogVersion, keyword)).isInstanceOf(
				UnknownIdentifierException.class);
	}


	@Test
	public void testTypecodeGetKeyward()
	{
		final KeywordModel keywordModel = new KeywordModel();
		Mockito.when(keywordDao.getKeywords(typecode, catalogVersion, keyword)).thenReturn(
				Collections.singletonList(keywordModel));

		Assertions.assertThat(keywordService.getKeyword(typecode, catalogVersion, keyword)).isSameAs(keywordModel);
	}


	@Test
	public void testTypecodeGetKeywardFailToMany()
	{

		Mockito.when(keywordDao.getKeywords(typecode, catalogVersion, keyword)).thenReturn(
				Arrays.asList(new KeywordModel(), new KeywordModel()));


		assertThatThrownBy(() -> keywordService.getKeyword(typecode, catalogVersion, keyword))
				.isInstanceOf(AmbiguousIdentifierException.class);
	}

	@Test
	public void testTypecodeGetKeywardFailEmpty()
	{

		Mockito.when(keywordDao.getKeywords(typecode, catalogVersion, keyword)).thenReturn(Collections.emptyList());

		assertThatThrownBy(() -> keywordService.getKeyword(typecode, catalogVersion, keyword)).isInstanceOf(
				UnknownIdentifierException.class);
	}


	@Test
	public void testTypecodeGetKeywardFailNullArg()
	{
		assertThatThrownBy(() -> keywordService.getKeyword(null, catalogVersion, keyword)).isInstanceOf(
				IllegalArgumentException.class);
		assertThatThrownBy(() -> keywordService.getKeyword(typecode, null, keyword)).isInstanceOf(IllegalArgumentException.class);
		assertThatThrownBy(() -> keywordService.getKeyword(typecode, catalogVersion, null)).isInstanceOf(
				IllegalArgumentException.class);
	}
}
