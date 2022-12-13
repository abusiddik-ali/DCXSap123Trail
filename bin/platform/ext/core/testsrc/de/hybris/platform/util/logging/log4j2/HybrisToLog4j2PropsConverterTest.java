/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util.logging.log4j2;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.util.Utilities;
import java.util.Properties;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

@UnitTest
public class HybrisToLog4j2PropsConverterTest
{


	private static final String LOG4J2 = "log4j2";
	private static final String APPENDER_CONSOLE_LAYOUT_PATTERN = "appender.console.layout.pattern";
	private static final String LOG4J2_APPENDER_CONSOLE_LAYOUT_PATTERN = LOG4J2 + "." + APPENDER_CONSOLE_LAYOUT_PATTERN;
	private static final String PATTERN_WITH_HIGHLIGHT_COLORS = "%highlight{%d [%t] %-5level: %msg%n%throwable}{FATAL=white, ERROR=red, WARN=blue, INFO=black, DEBUG=green, TRACE=blue}";
	private static final String PATTERN_WITH_HIGHLIGHT_DEFAULT = "%highlight{%d [%t] %-5level: %msg%n%throwable}";
	private static final String PATTERN_WITH_COLORS = "%black{%d [%t]} %cyan{%-5level:} %green{%msg%n%throwable}";
	private static final String PATTERN_WITH_HIGHLIGHT_STYLE = "%highlight{%d [%t] %-5level: %msg%n%throwable}{STYLE=Logback}";
	private static final String PATTERN_WITH_HIGHLIGHT_INLINE = "%d [%t] %highlight{%-5level: %msg%n%throwable}";
	private static final String PATTERN_WITH_HIGHLIGHT_INLINE_AND_STYLE = "%style{%d [%t]}{black} %highlight{%-5level: %msg%n%throwable}";
	private static final String PATTERN_WITH_STYLE = "%style{%d}{black} %style{[%t]}{blue} %style{%-5level:}{yellow} %style{%msg%n%throwable}{green}";
	private static final String PATTERN_WITHOUT_HIGHLIGHT = "%d [%t] %-5level: %msg%n%throwable";
	private final static String LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN = "log4j2.appender.jdbcConsoleLogger.layout.pattern";
	private final static String REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN = "remove." + LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN;
	private final static String REMOVE_LOG4J2_APPENDER_CONSOLE_LAYOUT_PATTERN = "remove.log4j2.appender.console.layout.pattern";
	private final static String APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_TYPE = "log4j2.appender.jdbcConsoleLogger.layout.type";
	private final static String APPENDER_CONSOLE_LAYOUT_TYPE = "log4j2.appender.console.layout.type";
	private final static String JSON_LAYOUT = "JsonLayout";
	private static final String LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN_VALUE = "%-5p [%t] %d{HH:mm:ss.SSS} [%X{jdbc-dataSourceId}|%X{jdbc-category}|%X{jdbc-timeElapsed} ms] %X{RemoteAddr} %X{Tenant} %m%n";
	final Properties platformProperties = Utilities.loadPlatformProperties();

	private final HybrisToLog4j2PropsConverter converter = new HybrisToLog4j2PropsConverter()
	{
		@Override
		public boolean isANSIDisable(final Properties platformProperties)
		{
			return true;
		}
	};

	private Properties testedProperties = new Properties();

	private final HybrisToLog4j2PropsConverter hybrisToLog4j2PropsConverter = new HybrisToLog4j2PropsConverter();
	private String toRemoveLog4j2 = "false";
	private String log4J2AppenderJdbcConsoleLoggerLayoutPatternBackup;

	@Before
	public void setUp()
	{
		log4J2AppenderJdbcConsoleLoggerLayoutPatternBackup = platformProperties.getProperty(LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN);
		if (log4J2AppenderJdbcConsoleLoggerLayoutPatternBackup == null)
		{
			platformProperties.put(LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN, LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN_VALUE);
		}
	}

	@After
	public void cleanup()
	{
		platformProperties.put(APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_TYPE, "PatternLayout");
		platformProperties.put(APPENDER_CONSOLE_LAYOUT_TYPE, "PatternLayout");
		if (log4J2AppenderJdbcConsoleLoggerLayoutPatternBackup == null)
		{
			platformProperties.remove(LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN);
		}
		platformProperties.remove(REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN);
		platformProperties.remove(REMOVE_LOG4J2_APPENDER_CONSOLE_LAYOUT_PATTERN);
	}

	@Test
	public void shouldUpdatePatternWhenAnsiDisabled()
	{
		// given
		final Properties properties = givenAnsiColorsAreDisabledWithLayoutPattern(
				HybrisToLog4j2PropsConverterTest.PATTERN_WITH_HIGHLIGHT_DEFAULT);
		// when
		final Properties log4jProperties = converter.convertToLog4jProps(properties);
		// then
		assertThatPatternIsWithoutHighlights(properties, log4jProperties);
	}

	@Test
	public void shouldNotUpdatePatternWithoutHighlightWhenAnsiDisabled()
	{
		// given
		final Properties properties = givenAnsiColorsAreDisabledWithLayoutPattern(PATTERN_WITHOUT_HIGHLIGHT);
		// when
		final Properties log4jProperties = converter.convertToLog4jProps(properties);
		// then
		assertThatPatternIsWithoutHighlights(properties, log4jProperties);
	}

	@Test
	public void shouldUpdatePatternWithColorsWhenAnsiDisabled()
	{
		// given
		final Properties properties = givenAnsiColorsAreDisabledWithLayoutPattern(
				HybrisToLog4j2PropsConverterTest.PATTERN_WITH_HIGHLIGHT_COLORS);
		// when
		final Properties log4jProperties = converter.convertToLog4jProps(properties);
		// then
		assertThatPatternIsWithoutHighlights(properties, log4jProperties);
	}

	@Test
	public void shouldUpdatePatternWithStyleWhenAnsiDisabled()
	{
		// given
		final Properties properties = givenAnsiColorsAreDisabledWithLayoutPattern(PATTERN_WITH_STYLE);
		// when
		final Properties log4jProperties = converter.convertToLog4jProps(properties);
		// then
		assertThatPatternIsWithoutHighlights(properties, log4jProperties);
	}

	@Test
	public void shouldUpdatePatternWithHighlightStyleWhenAnsiDisabled()
	{
		// given
		final Properties properties = givenAnsiColorsAreDisabledWithLayoutPattern(PATTERN_WITH_HIGHLIGHT_STYLE);
		// when
		final Properties log4jProperties = converter.convertToLog4jProps(properties);
		// then
		assertThatPatternIsWithoutHighlights(properties, log4jProperties);
	}

	@Test
	public void shouldUpdateInlinePatternWhenAnsiDisabled()
	{
		// given
		final Properties properties = givenAnsiColorsAreDisabledWithLayoutPattern(PATTERN_WITH_HIGHLIGHT_INLINE);
		// when
		final Properties log4jProperties = converter.convertToLog4jProps(properties);
		// then
		assertThatPatternIsWithoutHighlights(properties, log4jProperties);
	}

	@Test
	public void shouldUpdatePatternWithInlineAndStyleWhenAnsiDisabled()
	{
		// given
		final Properties properties = givenAnsiColorsAreDisabledWithLayoutPattern(PATTERN_WITH_HIGHLIGHT_INLINE_AND_STYLE);
		// when
		final Properties log4jProperties = converter.convertToLog4jProps(properties);
		// then
		assertThatPatternIsWithoutHighlights(properties, log4jProperties);
	}

	@Test
	public void shouldUpdatePatternWithColorStylesWhenAnsiDisabled()
	{
		// given
		final Properties properties = givenAnsiColorsAreDisabledWithLayoutPattern(PATTERN_WITH_COLORS);
		// when
		final Properties log4jProperties = converter.convertToLog4jProps(properties);
		// then
		assertThatPatternIsWithoutHighlights(properties, log4jProperties);
	}

	/**
	 * Sets console layout to given pattern and disables ANSI colors.
	 */
	protected Properties givenAnsiColorsAreDisabledWithLayoutPattern(String layout)
	{
		final Properties properties = new Properties();
		properties.put("ansi.colors", "false");
		properties.put("log4j2.appender.console.layout.pattern", layout);
		return properties;
	}

	/**
	 * Asserts that the console layout pattern does not include highlight syntax.
	 */
	protected void assertThatPatternIsWithoutHighlights(Properties hybrisProperties, Properties log4jProperties)
	{
		assertThat(hybrisProperties.getProperty("log4j2.appender.console.layout.pattern")).isEqualTo(PATTERN_WITHOUT_HIGHLIGHT);
		assertThat(log4jProperties.get("ansi.colors")).isNull();
		assertThat(log4jProperties.get("appender.console.layout.pattern")).isEqualTo(PATTERN_WITHOUT_HIGHLIGHT);
	}

	@Test
	public void shouldNotSetPatternWhenAnsiDisabledAndPatternNotSet()
	{
		// given
		final Properties properties = new Properties();
		properties.put("ansi.colors", "false");

		// when
		final Properties log4jProperties = converter.convertToLog4jProps(properties);

		// then
		assertThat(properties.getProperty("log4j2.appender.console.layout.pattern")).isNull();
		assertThat(log4jProperties.get("ansi.colors")).isNull();
		assertThat(log4jProperties.get("appender.console.layout.pattern")).isNull();
	}

	@Test
	public void shouldRemoveHighlightSyntaxWhenItCapturesWholeText()
	{
		convertAndAssertIfEqualTo("%highlight{%-5p [%t] %X{RemoteAddr}%X{Tenant}%X{CronJob}[%c{1}]}",
				"%-5p [%t] %X{RemoteAddr}%X{Tenant}%X{CronJob}[%c{1}]");
	}

	@Test
	public void shouldRemoveHighlightSyntaxWhenItCapturesBeginingOfText()
	{
		convertAndAssertIfEqualTo("%highlight{%-5p [%t] %X{RemoteAddr}}%X{Tenant}%X{CronJob}[%c{1}]",
				"%-5p [%t] %X{RemoteAddr}%X{Tenant}%X{CronJob}[%c{1}]");
	}

	@Test
	public void shouldRemoveHighlightSyntaxWhenItCapturesMiddleOfText()
	{
		convertAndAssertIfEqualTo("%-5p [%t] %highlight{%X{RemoteAddr}%X{Tenant}%X{CronJob}}[%c{1}]",
				"%-5p [%t] %X{RemoteAddr}%X{Tenant}%X{CronJob}[%c{1}]");
	}

	@Test
	public void shouldRemoveHighlightSyntaxWithStyleWhenItCapturesMiddleOfText()
	{
		convertAndAssertIfEqualTo("%-5p [%t] %highlight{%X{RemoteAddr}%X{Tenant}%X{CronJob}}{STYLE=Logback}[%c{1}]",
				"%-5p [%t] %X{RemoteAddr}%X{Tenant}%X{CronJob}[%c{1}]");
	}

	@Test
	public void shouldRemoveHighlightSyntaxWithStyleWhenItCapturesEndOfText()
	{
		convertAndAssertIfEqualTo("%-5p [%t] %X{RemoteAddr}%X{Tenant}%X{CronJob}%highlight{[%c{1}]}{STYLE=Logback}",
				"%-5p [%t] %X{RemoteAddr}%X{Tenant}%X{CronJob}[%c{1}]");
	}

	@Test
	public void shouldDoNothingWhenErrorInSyntax()
	{
		convertAndAssertIfEqualTo("%highlight{%-5p [%t] %X{RemoteAddr}%X{Tenant}%X{CronJob}{[%c{1}]",
				"%highlight{%-5p [%t] %X{RemoteAddr}%X{Tenant}%X{CronJob}{[%c{1}]");
	}

	@Test
	public void shouldDoNothingWhenErrorInSyntax2()
	{
		convertAndAssertIfEqualTo("%highlight%-5p [%t] %X{RemoteAddr}%X{Tenant}%X{CronJob}}[%c{1}]",
				"%highlight%-5p [%t] %X{RemoteAddr}%X{Tenant}%X{CronJob}}[%c{1}]");
	}


	private void convertAndAssertIfEqualTo(final String convertFrom, final String compareTo)
	{
		final Properties props = new Properties();

		props.put(LOG4J2_APPENDER_CONSOLE_LAYOUT_PATTERN, convertFrom);

		final Properties convertToLog4jProps = converter.convertToLog4jProps(props);

		assertThat(convertToLog4jProps.get(APPENDER_CONSOLE_LAYOUT_PATTERN)).isEqualTo(compareTo);


	}

	@Test
	public void shouldReturnAllProperties()
	{
		//given
		platformProperties.put(REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN, "false");
		platformProperties.put(REMOVE_LOG4J2_APPENDER_CONSOLE_LAYOUT_PATTERN, "false");
		//when
		testedProperties = hybrisToLog4j2PropsConverter.removeLog4jProps(platformProperties);
		//then
		assertThat(testedProperties).isEqualTo(platformProperties);
	}

	@Test
	public void shouldReturnPropertiesWithRemoveLog4jPropertyAndChangedLayoutType()
	{
		//given
		toRemoveLog4j2 = "true";
		platformProperties.put(REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN, toRemoveLog4j2);
		platformProperties.put(APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_TYPE, JSON_LAYOUT);
		assertThat(platformProperties.getProperty(LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN)).isNotNull();

		//when
		testedProperties = hybrisToLog4j2PropsConverter.removeLog4jProps(platformProperties);

		//then
		assertThat(testedProperties.getProperty(LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN)).isNull();
	}

	@Test
	public void shouldReturnPropertiesWithoutRemoveLog4jProperty()
	{
		//given
		toRemoveLog4j2 = "true";
		platformProperties.put(REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN,toRemoveLog4j2);
		final String removeProps = REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN.replace("remove.", "");
		assertThat(platformProperties.getProperty(REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN)).isEqualTo("true");

		//when
		testedProperties = hybrisToLog4j2PropsConverter.removeLog4jProps(platformProperties);
		//then
		assertThat(testedProperties.getProperty(removeProps)).isNull();
	}

	@Test
	public void shouldReturnPropertiesWithout2RemoveLog4jProperties()
	{
		//given
		toRemoveLog4j2 = "true";
		platformProperties.put(REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN,toRemoveLog4j2);
		platformProperties.put(REMOVE_LOG4J2_APPENDER_CONSOLE_LAYOUT_PATTERN,toRemoveLog4j2);

		final String removePropsJDBC = REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN.replace("remove.", "");
		final String removePropsConsole = REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN.replace("remove.", "");

		assertThat(platformProperties.getProperty(REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN)).isEqualTo("true");
		assertThat(platformProperties.getProperty(REMOVE_LOG4J2_APPENDER_CONSOLE_LAYOUT_PATTERN)).isEqualTo("true");

		//when
		testedProperties = hybrisToLog4j2PropsConverter.removeLog4jProps(platformProperties);
		//then
		assertThat(testedProperties.getProperty(removePropsJDBC)).isNull();
		assertThat(testedProperties.getProperty(removePropsConsole)).isNull();
	}

	@Test
	public void shouldReturnPropertiesForLog4j2()
	{
		//given
		toRemoveLog4j2 = "true";
		platformProperties.put(REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN,toRemoveLog4j2);
		platformProperties.put(APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_TYPE, JSON_LAYOUT);

		final String removeProps = REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN.replace("remove.", "");
		assertThat(platformProperties.getProperty(REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN)).isEqualTo("true");

		//when
		testedProperties = hybrisToLog4j2PropsConverter.convertToLog4jProps(platformProperties);

		//then
		assertThat(testedProperties).isNotIn(platformProperties);
		assertThat(testedProperties.getProperty(removeProps)).isNull();
		assertThat(testedProperties.keys()).isNotEqualTo(platformProperties.keys());
	}

	@Test
	public void shouldReturnPropertiesForLog4j2With2Remove()
	{
		//given
		toRemoveLog4j2 = "true";
		platformProperties.put(REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN,toRemoveLog4j2);
		platformProperties.put(REMOVE_LOG4J2_APPENDER_CONSOLE_LAYOUT_PATTERN,toRemoveLog4j2);

		platformProperties.put(APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_TYPE, JSON_LAYOUT);
		platformProperties.put(APPENDER_CONSOLE_LAYOUT_TYPE, JSON_LAYOUT);

		final String removePropsJDBC = REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN.replace("remove.", "");
		final String removePropsConsole = REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN.replace("remove.", "");

		assertThat(platformProperties.getProperty(REMOVE_LOG4J2_APPENDER_JDBC_CONSOLE_LOGGER_LAYOUT_PATTERN)).isEqualTo("true");
		assertThat(platformProperties.getProperty(REMOVE_LOG4J2_APPENDER_CONSOLE_LAYOUT_PATTERN)).isEqualTo("true");

		//when
		testedProperties = hybrisToLog4j2PropsConverter.convertToLog4jProps(platformProperties);

		//then
		assertThat(testedProperties).isNotIn(platformProperties);
		assertThat(testedProperties.getProperty(removePropsJDBC)).isNull();
		assertThat(testedProperties.keys()).isNotEqualTo(platformProperties.keys());

		assertThat(testedProperties).isNotIn(platformProperties);
		assertThat(testedProperties.getProperty(removePropsConsole)).isNull();
		assertThat(testedProperties.keys()).isNotEqualTo(platformProperties.keys());
	}
}
