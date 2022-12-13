/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl.gateways;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.type.TypeService;

import java.time.Duration;
import java.util.Locale;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.jdbc.core.JdbcTemplate;

@IntegrationTest
public class LocaleAwareTasksQueueGatewayTest extends BaseGatewayTest
{
	@Resource
	private TypeService typeService;

	@Resource
	private JdbcTemplate jdbcTemplate;

	private final Locale defaultLocale = Locale.getDefault();

	@Before
	public void setUp() throws Exception
	{
		Locale.setDefault(new Locale("ar", "BH"));
	}

	@After
	public void cleanUpLocale()
	{
		Locale.setDefault(defaultLocale);
	}

	public void defaultIfNullShouldReturnCorrectlyFormattedMessage(TasksQueueGateway gateway){
		String statement = gateway.defaultIfNull("p_nodeId", -1);
		assertThat(statement).doesNotContain("١");
		assertThat(statement).contains("-1");
	}

	public void getCleanTasksQueueStatementShouldReturnCorrectlyFormattedMessage(String cleanTasksQueueStatement){
		assertThat(cleanTasksQueueStatement).doesNotContain("٣٦");
		assertThat(cleanTasksQueueStatement).contains("3600");
	}

	@Test
	public void msSqlDefaultIfNullShouldReturnCorrectlyFormattedMessage(){
		MsSqlTasksQueueGateway gateway = new MsSqlTasksQueueGateway(jdbcTemplate, typeService);
		defaultIfNullShouldReturnCorrectlyFormattedMessage(gateway);
	}

	@Test
	public void msSqlGetCleanTasksQueueStatementShouldReturnCorrectlyFormattedMessage(){
		MsSqlTasksQueueGateway gateway = new MsSqlTasksQueueGateway(jdbcTemplate, typeService);
		getCleanTasksQueueStatementShouldReturnCorrectlyFormattedMessage(gateway.getCleanTasksQueueStatement(Duration.ofHours(1)));
	}

	@Test
	public void mySqlDefaultIfNullShouldReturnCorrectlyFormattedMessage(){
		MySqlTasksQueueGateway gateway = new MySqlTasksQueueGateway(jdbcTemplate, typeService);
		defaultIfNullShouldReturnCorrectlyFormattedMessage(gateway);
	}

	@Test
	public void mySqlGetCleanTasksQueueStatementShouldReturnCorrectlyFormattedMessage(){
		MySqlTasksQueueGateway gateway = new MySqlTasksQueueGateway(jdbcTemplate, typeService);
		getCleanTasksQueueStatementShouldReturnCorrectlyFormattedMessage(gateway.getCleanTasksQueueStatement(Duration.ofHours(1)));
	}

	@Test
	public void postgresDefaultIfNullShouldReturnCorrectlyFormattedMessage(){
		PostgresTasksQueueGateway gateway = new PostgresTasksQueueGateway(jdbcTemplate, typeService);
		defaultIfNullShouldReturnCorrectlyFormattedMessage(gateway);
	}

	@Test
	public void postgresGetCleanTasksQueueStatementShouldReturnCorrectlyFormattedMessage(){
		PostgresTasksQueueGateway gateway = new PostgresTasksQueueGateway(jdbcTemplate, typeService);
		getCleanTasksQueueStatementShouldReturnCorrectlyFormattedMessage(gateway.getCleanTasksQueueStatement(Duration.ofHours(1)));
	}

	@Test
	public void oracleDefaultIfNullShouldReturnCorrectlyFormattedMessage(){
		OracleTasksQueueGateway gateway = new OracleTasksQueueGateway(jdbcTemplate, typeService);
		defaultIfNullShouldReturnCorrectlyFormattedMessage(gateway);
	}

	@Test
	public void oracleGetCleanTasksQueueStatementShouldReturnCorrectlyFormattedMessage(){
		OracleTasksQueueGateway gateway = new OracleTasksQueueGateway(jdbcTemplate, typeService);
		getCleanTasksQueueStatementShouldReturnCorrectlyFormattedMessage(gateway.getCleanTasksQueueStatement(Duration.ofHours(1)));
	}

	@Test
	public void hanaDefaultIfNullShouldReturnCorrectlyFormattedMessage(){
		HanaTasksQueueGateway gateway = new HanaTasksQueueGateway(jdbcTemplate, typeService);
		defaultIfNullShouldReturnCorrectlyFormattedMessage(gateway);
	}

	@Test
	public void hanaGetCleanTasksQueueStatementShouldReturnCorrectlyFormattedMessage(){
		HanaTasksQueueGateway gateway = new HanaTasksQueueGateway(jdbcTemplate, typeService);
		getCleanTasksQueueStatementShouldReturnCorrectlyFormattedMessage(gateway.getCleanTasksQueueStatement(Duration.ofHours(1)));
	}
}
