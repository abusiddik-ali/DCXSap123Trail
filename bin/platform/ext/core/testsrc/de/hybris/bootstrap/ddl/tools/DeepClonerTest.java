/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.bootstrap.ddl.tools;

import org.apache.ddlutils.model.ForeignKey;
import org.apache.ddlutils.model.Table;
import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;


public class DeepClonerTest
{

	@Test
	public void shouldCloneSerializableObject() throws Exception
	{
		// given
		final DeepCloner<Table> deepCloner = new DeepCloner<>();
		final Table table = new Table();
		table.setName("fooBar");

		// when
		final Table clonedTable = deepCloner.cloneDeeply(table);

		// then
		assertThat(table).isNotSameAs(clonedTable);
		assertThat(table).isEqualTo(clonedTable);
	}

	@Test
	public void shouldCloneNotSerializableObject() throws Exception
	{
		// given
		final DeepCloner<ForeignKey> deepCloner = new DeepCloner<>();
		final ForeignKey foreignKey = new ForeignKey();
		foreignKey.setName("fooBar");

		// when
		final ForeignKey clonedForeignKey = deepCloner.cloneDeeply(foreignKey);

		// then
		assertThat(foreignKey).isNotSameAs(clonedForeignKey);
		assertThat(foreignKey).isEqualTo(clonedForeignKey);
	}

}
