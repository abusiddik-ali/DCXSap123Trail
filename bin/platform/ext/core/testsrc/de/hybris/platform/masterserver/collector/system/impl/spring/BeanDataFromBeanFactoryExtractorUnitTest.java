/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.masterserver.collector.system.impl.spring;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;

import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.context.support.ClassPathXmlApplicationContext;

@UnitTest
public class BeanDataFromBeanFactoryExtractorUnitTest
{
	private static final String TEST_EXTENSION_NAME = "test-extension";
	private static final Path TEST_PATH = Path.of("test-path");

	@Test
	public void shouldNotFailOnMissingBean()
	{
		final ConfigurableListableBeanFactory beanFactory = givenBeanFactory();
		final BeanDataFromBeanFactoryExtractor extractor = givenExtractor(beanFactory);
		final String notExistingBeanName = "notExisting";

		final Optional<Class<?>> type = extractor.getDeclaredType(notExistingBeanName);
		assertThat(type).isEmpty();

		final Collection<String> aliases = extractor.getDeclaredAliases(notExistingBeanName);
		assertThat(aliases).isNotNull().isEmpty();

		final Optional<String> extensionName = extractor.getDeclaringExtensionName(notExistingBeanName);
		assertThat(extensionName).isEmpty();
	}

	@Test
	public void shouldExtractDataForAnExistingBeanWithoutAliases()
	{
		final ConfigurableListableBeanFactory beanFactory = givenBeanFactory();
		final BeanDataFromBeanFactoryExtractor extractor = givenExtractor(beanFactory);
		final String existingBeanName = "existingNoAliases";

		final Optional<Class<?>> type = extractor.getDeclaredType(existingBeanName);
		assertThat(type).contains(String.class);

		final Collection<String> aliases = extractor.getDeclaredAliases(existingBeanName);
		assertThat(aliases).isNotNull().isEmpty();

		final Optional<String> extensionName = extractor.getDeclaringExtensionName(existingBeanName);
		assertThat(extensionName).isNotNull().contains(TEST_EXTENSION_NAME);
	}

	@Test
	public void shouldExtractDataForAnExistingBeanWithAliases()
	{
		final ConfigurableListableBeanFactory beanFactory = givenBeanFactory();
		final BeanDataFromBeanFactoryExtractor extractor = givenExtractor(beanFactory);
		final String existingBeanName = "existingWithAliases";

		final Optional<Class<?>> type = extractor.getDeclaredType(existingBeanName);
		assertThat(type).contains(String.class);

		final Collection<String> aliases = extractor.getDeclaredAliases(existingBeanName);
		assertThat(aliases).isNotNull().isNotEmpty().containsOnly("existingWithAliases1", "existingWithAliases2");

		final Optional<String> extensionName = extractor.getDeclaringExtensionName(existingBeanName);
		assertThat(extensionName).isNotNull().contains(TEST_EXTENSION_NAME);
	}

	@Test
	public void shouldExtractDataForAnAbstractBean()
	{
		final ConfigurableListableBeanFactory beanFactory = givenBeanFactory();
		final BeanDataFromBeanFactoryExtractor extractor = givenExtractor(beanFactory);
		final String abstractBeanName = "abstractBean";

		final Optional<Class<?>> type = extractor.getDeclaredType(abstractBeanName);
		assertThat(type).contains(String.class);

		final Collection<String> aliases = extractor.getDeclaredAliases(abstractBeanName);
		assertThat(aliases).isNotNull().isEmpty();

		final Optional<String> extensionName = extractor.getDeclaringExtensionName(abstractBeanName);
		assertThat(extensionName).isNotNull().contains(TEST_EXTENSION_NAME);
	}

	@Test
	public void shouldExtractDataFromAListBean()
	{
		final ConfigurableListableBeanFactory beanFactory = givenBeanFactory();
		final BeanDataFromBeanFactoryExtractor extractor = givenExtractor(beanFactory);
		final String listBean = "listBean";

		final Optional<Class<?>> type = extractor.getDeclaredType(listBean);
		assertThat(type).contains(List.class);

		final Collection<String> aliases = extractor.getDeclaredAliases(listBean);
		assertThat(aliases).isNotNull().isEmpty();

		final Optional<String> extensionName = extractor.getDeclaringExtensionName(listBean);
		assertThat(extensionName).isNotNull().contains(TEST_EXTENSION_NAME);
	}

	@Test
	public void shouldExtractDataFromASetBean()
	{
		final ConfigurableListableBeanFactory beanFactory = givenBeanFactory();
		final BeanDataFromBeanFactoryExtractor extractor = givenExtractor(beanFactory);
		final String setBean = "setBean";

		final Optional<Class<?>> type = extractor.getDeclaredType(setBean);
		assertThat(type).contains(Set.class);

		final Collection<String> aliases = extractor.getDeclaredAliases(setBean);
		assertThat(aliases).isNotNull().isEmpty();

		final Optional<String> extensionName = extractor.getDeclaringExtensionName(setBean);
		assertThat(extensionName).isNotNull().contains(TEST_EXTENSION_NAME);
	}

	@Test
	public void shouldExtractDataFromAMapBean()
	{
		final ConfigurableListableBeanFactory beanFactory = givenBeanFactory();
		final BeanDataFromBeanFactoryExtractor extractor = givenExtractor(beanFactory);
		final String mapBean = "mapBean";

		final Optional<Class<?>> type = extractor.getDeclaredType(mapBean);
		assertThat(type).contains(Map.class);

		final Collection<String> aliases = extractor.getDeclaredAliases(mapBean);
		assertThat(aliases).isNotNull().isEmpty();

		final Optional<String> extensionName = extractor.getDeclaringExtensionName(mapBean);
		assertThat(extensionName).isNotNull().contains(TEST_EXTENSION_NAME);
	}

	@Test
	public void shouldExtractDataFromLazyBeanWithoutInstantiatingIt()
	{
		final ConfigurableListableBeanFactory beanFactory = givenBeanFactory();
		final BeanDataFromBeanFactoryExtractor extractor = givenExtractor(beanFactory);
		final String lazyBean = "lazyBean";

		assertThat(InstantiationTest.getNumberOfInstances()).isZero();

		final Optional<Class<?>> type = extractor.getDeclaredType(lazyBean);
		assertThat(type).contains(InstantiationTest.class);

		final Collection<String> aliases = extractor.getDeclaredAliases(lazyBean);
		assertThat(aliases).isNotNull().isNotEmpty().containsOnly("lazyBean1", "lazyBean2");

		final Optional<String> extensionName = extractor.getDeclaringExtensionName(lazyBean);
		assertThat(extensionName).isNotNull().contains(TEST_EXTENSION_NAME);

		assertThat(InstantiationTest.getNumberOfInstances()).isZero();
	}

	@Test
	public void shouldSkipBeansWithMissingClasses()
	{
		final ConfigurableListableBeanFactory beanFactory = givenBeanFactory();
		final BeanDataFromBeanFactoryExtractor extractor = givenExtractor(beanFactory);
		final String notDefinedClassBean = "notDefinedClassBean";

		final Optional<Class<?>> type = extractor.getDeclaredType(notDefinedClassBean);
		assertThat(type).isEmpty();

		final Collection<String> aliases = extractor.getDeclaredAliases(notDefinedClassBean);
		assertThat(aliases).isNotNull().isEmpty();

		final Optional<String> extensionName = extractor.getDeclaringExtensionName(notDefinedClassBean);
		assertThat(extensionName).isNotNull().contains(TEST_EXTENSION_NAME);
	}

	private ConfigurableListableBeanFactory givenBeanFactory()
	{
		return new ClassPathXmlApplicationContext("/test/BeanDataFromBeanFactoryExtractorUnitTest-context.xml").getBeanFactory();
	}

	private BeanDataFromBeanFactoryExtractor givenExtractor(final ConfigurableListableBeanFactory beanFactory)
	{
		return new BeanDataFromBeanFactoryExtractor(
				beanFactory,
				path -> Optional.of(TEST_EXTENSION_NAME),
				beanDefinition -> Optional.of(TEST_PATH)
		);
	}

	static class InstantiationTest
	{
		private static final AtomicInteger INSTANTIATION_COUNTER = new AtomicInteger();

		public InstantiationTest()
		{
			INSTANTIATION_COUNTER.incrementAndGet();
		}

		public static int getNumberOfInstances()
		{
			return INSTANTIATION_COUNTER.get();
		}
	}
}