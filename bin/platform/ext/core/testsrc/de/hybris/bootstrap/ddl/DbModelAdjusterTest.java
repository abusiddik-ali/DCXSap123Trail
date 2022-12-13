/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.bootstrap.ddl;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.bootstrap.ddl.DbModelAdjuster.IndexEquivalence;
import de.hybris.bootstrap.ddl.model.YDbTableProvider;
import de.hybris.bootstrap.ddl.model.YTable;
import de.hybris.bootstrap.ddl.sql.DBAwareNonUniqueIndexExtended;
import de.hybris.bootstrap.ddl.sql.DbAwareUniqueIndexExtended;
import de.hybris.bootstrap.ddl.sql.ExtendedAwareIndex;
import de.hybris.bootstrap.ddl.sql.MSSqlExtendedParamsForIndex;
import de.hybris.platform.testframework.Assert;

import static org.assertj.core.api.Assertions.assertThat;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.ddlutils.model.Index;
import org.apache.ddlutils.model.IndexColumn;
import org.apache.ddlutils.model.Table;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

/**
 * Tests for {@link DbModelAdjuster} and contained inner classes.
 */
@UnitTest
public class DbModelAdjusterTest
{
	@Mock
	private YDbTableProvider tableProvider;

	@Before
	public void setUp()
	{
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void allInstancesOfIndexEquivalenceShouldAlwaysBeEqual()
	{
		// DbModelAdjuster instance required for inner class
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);

		final IndexEquivalence equivalence1 = adjuster.new IndexEquivalence();
		final IndexEquivalence equivalence2 = adjuster.new IndexEquivalence();

		Assert.assertEquals(equivalence1, equivalence2);
	}

	@Test
	public void shouldDropAllIndicesFromExistingTable()
	{
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);
		final Table existingTable = new Table();
		final YTable desiredTable = Mockito.mock(YTable.class);

		final DBAwareNonUniqueIndexExtended existingExtended = new DBAwareNonUniqueIndexExtended();
		existingExtended.addColumn(new IndexColumn("col1"));
		existingTable.addIndex(existingExtended);

		Mockito.when(desiredTable.getIndices()).thenReturn(new Index[]{});
		adjuster.adjustIndices(existingTable, desiredTable);
		assertThat(existingTable.getIndices()).isEmpty();
	}

	@Test
	public void shouldReturnExistingIndexForExistingTableWhenNothingChanged()
	{
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);
		final Table existingTable = new Table();
		final YTable desiredTable = new YTable("test");

		final Index existingExtended = createIndex(Pair.of(new String[]{ "col1" }, new String[]{}));
		existingTable.addIndex(existingExtended);

		final Index desiredExtended = createIndex(Pair.of(new String[]{ "col1" }, new String[]{}));
		desiredTable.addIndex(desiredExtended);

		adjuster.adjustIndices(existingTable, desiredTable);

		assertThat(existingTable.getIndices()).isNotEmpty();
		assertThat(existingTable.getIndices()).hasSize(1);
		assertThat(existingTable.getIndices()).contains(existingExtended);
	}


	@Test
	public void shouldReturnExistingIndexForExistingTableWhenNothingChangedExceptColumnNameCase()
	{
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);
		final Table existingTable = new Table();
		final YTable desiredTable = new YTable("test");

		final Index existingExtended = createIndex(Pair.of(new String[]{ "col1" }, new String[]{}));
		existingTable.addIndex(existingExtended);

		final Index desiredExtended = createIndex(Pair.of(new String[]{ "COL1" }, new String[]{}));
		desiredTable.addIndex(desiredExtended);

		adjuster.adjustIndices(existingTable, desiredTable);
		assertThat(existingTable.getIndices()).isNotEmpty();
		assertThat(existingTable.getIndices()).hasSize(1);
		assertThat(existingTable.getIndices()).contains(existingExtended);
	}

	@Test
	public void shouldReturnExistingIndexForExistingTableWithIncludeSectionWhenNothingChangedExceptColumnNameCaseForInclude()
	{
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);
		final Table existingTable = new Table();
		final YTable desiredTable = new YTable("test");

		final Index existingExtended = createIndex(
				Pair.of(new String[]{ "col1" }, new String[]{ "inclCol1", "inclCol2" }));
		existingTable.addIndex(existingExtended);

		final Index desiredExtended = createIndex(
				Pair.of(new String[]{ "COL1" }, new String[]{ "INCLCol1", "INCLCol2" }));
		desiredTable.addIndex(desiredExtended);

		adjuster.adjustIndices(existingTable, desiredTable);
		assertThat(existingTable.getIndices()).isNotEmpty();
		assertThat(existingTable.getIndices()).hasSize(1);
		assertThat(existingTable.getIndices()).contains(existingExtended);
	}


	@Test
	public void shouldReturnExistingIndexForExistingTableWhenNothingChangedExceptColumnOrderForIncludeSection()
	{
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);
		final Table existingTable = new Table();
		final YTable desiredTable = new YTable("test");

		final Index existingExtended = createIndex(
				Pair.of(new String[]{ "col1" }, new String[]{ "inclCol1", "inclCol2" }));
		existingTable.addIndex(existingExtended);

		final Index desiredExtended = createIndex(
				Pair.of(new String[]{ "COL1" }, new String[]{ "inclCol2", "inclCol1" }));
		desiredTable.addIndex(desiredExtended);

		adjuster.adjustIndices(existingTable, desiredTable);
		assertThat(existingTable.getIndices()).isNotEmpty();
		assertThat(existingTable.getIndices()).hasSize(1);
		assertThat(existingTable.getIndices()).contains(existingExtended);
	}

	@Test
	public void shouldReturnNewIndexForExistingTableWhenIncludeColumnsForIndexChanged()
	{
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);
		final Table existingTable = new Table();
		final YTable desiredTable = new YTable("test");

		final Index existingExtended = createIndex(
				Pair.of(new String[]{ "col1" }, new String[]{ "inclCol1", "inclCol2" }));
		existingTable.addIndex(existingExtended);

		final Index desiredExtended = createIndex(
				Pair.of(new String[]{ "col1" }, new String[]{ "inclCol1", "inclCol2", "inclCol3" }));
		desiredTable.addIndex(desiredExtended);

		adjuster.adjustIndices(existingTable, desiredTable);
		assertThat(existingTable.getIndices()).isNotEmpty();
		assertThat(existingTable.getIndices()).contains(desiredExtended);
		assertThat(existingTable.getIndices()).hasSize(1);
		assertThat(existingTable.getIndices()).doesNotContain(existingExtended);
	}

	@Test
	public void shouldReturnDesiredIndexForExistingTableWhenIncludeColumnsAreRemoved()
	{
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);
		final Table existingTable = new Table();
		final YTable desiredTable = new YTable("test");

		final Index existingExtended = createIndex(
				Pair.of(new String[]{ "col1" }, new String[]{ "inclCol1", "inclCol2" }));
		existingTable.addIndex(existingExtended);

		final Index desiredExtended = createIndex(Pair.of(new String[]{ "COL1" }, new String[]{}));
		desiredTable.addIndex(desiredExtended);

		adjuster.adjustIndices(existingTable, desiredTable);
		assertThat(existingTable.getIndices()).isNotEmpty();
		assertThat(existingTable.getIndices()).contains(desiredExtended);
		assertThat(existingTable.getIndices()).hasSize(1);
		assertThat(existingTable.getIndices()).doesNotContain(existingExtended);
	}

	@Test
	public void shouldReturnDesiredIndexForExistingTableWhenIncludeColumnsAreRemoved2()
	{
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);
		final Table existingTable = new Table();
		final YTable desiredTable = new YTable("test");

		final Index existingIndex = createIndex(Pair.of(new String[]{ "col1" }, new String[]{ "inclCol1", "inclCol2" }));
		existingTable.addIndex(existingIndex);

		final Index desiredIndex = createIndex(Pair.of(new String[]{ "COL1" }, new String[]{}));
		desiredTable.addIndex(desiredIndex);

		adjuster.adjustIndices(existingTable, desiredTable);
		assertThat(existingTable.getIndices()).isNotEmpty();
		assertThat(existingTable.getIndices()).contains(desiredIndex);
		assertThat(existingTable.getIndices()).hasSize(1);
		assertThat(existingTable.getIndices()).doesNotContain(existingIndex);
	}

	@Test
	public void shouldReturnDesiredIndexForExistingTableWhenUniquenessChanged()
	{
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);
		final Table existingTable = new Table();
		final YTable desiredTable = new YTable("test");

		final Index existingIndex = createIndex(Pair.of(new String[]{ "col1" }, new String[]{}));
		existingTable.addIndex(existingIndex);

		final Index desiredIndex = createIndex(Pair.of(new String[]{ "col1" }, new String[]{}), true);
		desiredTable.addIndex(desiredIndex);

		adjuster.adjustIndices(existingTable, desiredTable);
		assertThat(existingTable.getIndices()).isNotEmpty();
		assertThat(existingTable.getIndices()).contains(desiredIndex);
		assertThat(existingTable.getIndices()).hasSize(1);
		assertThat(existingTable.getIndices()).doesNotContain(existingIndex);
	}

	@Test
	public void shouldReturnDesiredIndexWithIncludeForExistingTableWhenUniquenessChanged()
	{
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);
		final Table existingTable = new Table();
		final YTable desiredTable = new YTable("test");

		final Index existingIndex = createIndex(Pair.of(new String[]{ "col1" }, new String[]{ "col2" }));
		existingTable.addIndex(existingIndex);

		final Index desiredIndex = createIndex(Pair.of(new String[]{ "col1" }, new String[]{ "col2" }), true);
		desiredTable.addIndex(desiredIndex);

		adjuster.adjustIndices(existingTable, desiredTable);
		assertThat(existingTable.getIndices()).hasSize(1);
		assertThat(existingTable.getIndices()).contains(desiredIndex);
		assertThat(existingTable.getIndices()).doesNotContain(existingIndex);
	}


	@Test
	public void shouldRemoveExistingIndexIfNotFoundInDesiredTable()
	{
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);
		final Table existingTable = new Table();
		final YTable desiredTable = new YTable("test");


		existingTable.addIndex(createIndex("ex1", Pair.of(new String[]{ "col1" }, new String[]{ "col2" })));
		existingTable.addIndex(createIndex("ex2", Pair.of(new String[]{ "col1" }, new String[]{ "col3" })));
		desiredTable.addIndex(createIndex("des3", Pair.of(new String[]{ "col1" }, new String[]{ "col2" })));

		adjuster.adjustIndices(existingTable, desiredTable);
		assertThat(existingTable.getIndices()).hasSize(1);
		assertThat(existingTable.getIndices()).extracting(Index::getName).doesNotContain("ex2");
		assertThat(existingTable.getIndices()).extracting(Index::getName).contains("ex1");
	}


	@Test
	public void shouldReturnExistingIndicesIfAllAreFoundInDesiredTable()
	{
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);
		final Table existingTable = new Table();
		final YTable desiredTable = new YTable("test");


		existingTable.addIndex(createIndex("ex1", Pair.of(new String[]{ "col1" }, new String[]{ "col2" })));
		existingTable.addIndex(createIndex("ex2", Pair.of(new String[]{ "col1" }, new String[]{ "col3" })));
		desiredTable.addIndex(createIndex("des3", Pair.of(new String[]{ "col1" }, new String[]{ "col2" })));
		desiredTable.addIndex(createIndex("des4", Pair.of(new String[]{ "col1" }, new String[]{ "col3" })));

		adjuster.adjustIndices(existingTable, desiredTable);
		assertThat(existingTable.getIndices()).hasSize(2);
		assertThat(existingTable.getIndices()).extracting(Index::getName).contains("ex1", "ex2");
	}

	@Test
	public void shouldReturnAdditionalDesiredIndexIfNotExistInExistingTable()
	{
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);
		final Table existingTable = new Table();
		final YTable desiredTable = new YTable("test");


		existingTable.addIndex(createIndex("ex1", Pair.of(new String[]{ "col1" }, new String[]{ "col2" })));
		desiredTable.addIndex(createIndex("des2", Pair.of(new String[]{ "col1" }, new String[]{ "col2" })));
		desiredTable.addIndex(createIndex("des3", Pair.of(new String[]{ "col1" }, new String[]{ "col3" })));

		adjuster.adjustIndices(existingTable, desiredTable);
		assertThat(existingTable.getIndices()).hasSize(2);
		assertThat(existingTable.getIndices()).extracting(Index::getName).contains("ex1", "des3");
	}


	@Test
	public void shouldReturnExistingIndicesIfAllAreFoundInDesiredTableAndIncludeColumnsAreMixedWihtCoreIndexColumns()
	{
		final DbModelAdjuster adjuster = new DbModelAdjuster(tableProvider);
		final Table existingTable = new Table();
		final YTable desiredTable = new YTable("test");


		existingTable.addIndex(createIndex("ex1", Pair.of(new String[]{ "col1" }, new String[]{ "col2" })));
		existingTable.addIndex(createIndex("ex2", Pair.of(new String[]{ "col2" }, new String[]{ "col1" })));
		desiredTable.addIndex(createIndex("des3", Pair.of(new String[]{ "col1" }, new String[]{ "col2" })));
		desiredTable.addIndex(createIndex("des4", Pair.of(new String[]{ "col2" }, new String[]{ "col1" })));
		adjuster.adjustIndices(existingTable, desiredTable);
		assertThat(existingTable.getIndices()).hasSize(2);
		assertThat(existingTable.getIndices()).extracting(Index::getName).contains("ex1", "ex2");
	}

	private Index createIndex(final String indexName, final Pair<String[], String[]> params, final boolean unique)
	{
		final Index index;
		if (unique)
		{
			index = new DbAwareUniqueIndexExtended();
		}
		else
		{
			index = new DBAwareNonUniqueIndexExtended();
		}
		index.setName(indexName);

		for (final String coreCol : params.getKey())
		{
			index.addColumn(new IndexColumn(coreCol));
		}
		final MSSqlExtendedParamsForIndex extendedParamsForIndex = new MSSqlExtendedParamsForIndex();
		for (final String inclCol : params.getValue())
		{
			extendedParamsForIndex.addColumn(new IndexColumn(inclCol));
		}
		((ExtendedAwareIndex) index).setExtendedParams(extendedParamsForIndex);
		return index;
	}

	private Index createIndex(final Pair<String[], String[]> params, final boolean unique)
	{
		return createIndex("dummyIndexName", params, unique);
	}

	private Index createIndex(final String indexName, final Pair<String[], String[]> params)
	{
		return createIndex(indexName, params, false);
	}

	private Index createIndex(final Pair<String[], String[]> params)
	{
		return createIndex(params, false);
	}


}
