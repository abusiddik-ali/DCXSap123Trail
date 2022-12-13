/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util.database;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.jdbcwrapper.HybrisDataSource;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.util.Config;

import org.hamcrest.Matchers;
import org.junit.Assume;
import org.junit.Test;
import org.springframework.jdbc.core.JdbcTemplate;

@IntegrationTest
public class HANAApplicationSessionVariableTest extends ServicelayerBaseTest
{

	private static final String SELECT_SESSION_CONTEXT = "SELECT SESSION_CONTEXT('APPLICATION') FROM dummy;";

	@Test
	public void checkApplicationPropertyIsSetForHanaDatabase()
	{
		//given
		Assume.assumeThat(Config.getDatabaseName().getName(), Matchers.equalTo(Config.DatabaseNames.HANA));

		//when
		final HybrisDataSource dataSource = Registry.getCurrentTenant().getDataSource();
		final JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);
		final String result = jdbcTemplate.queryForObject(SELECT_SESSION_CONTEXT, String.class);

		//then
		assertThat(dataSource.getConnectionParameters().get("db.url")).containsOnlyOnce("SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA");
		assertThat(result).isEqualTo("SAP_COMMERCE_HRA");
	}
}
