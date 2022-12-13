/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.masterserver.collector.system.impl.spring;

import static de.hybris.platform.masterserver.collector.system.impl.spring.JsonSerializer.BEAN_NAME_KEY;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.constants.CoreConstants;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.junit.Test;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@IntegrationTest
public class SpringConfigOverviewProviderIntegrationTest extends ServicelayerBaseTest
{
	@Test
	public void shouldContainBeanFromGlobalContext()
	{
		final BeanInfo globalBean = requireBean("globalHybrisProperties");
		assertThat(globalBean.getType()).isNotNull().isEqualTo(java.util.Properties.class);
		assertThat(globalBean.getExtensionName()).isEqualTo(CoreConstants.EXTENSIONNAME);
		assertThat(globalBean.getAliases()).isNotNull().isNotEmpty().contains("hybrisProperties");
	}

	@Test
	public void shouldContainBeanFromApplicationContext()
	{
		final BeanInfo globalBean = requireBean("applicationHybrisProperties");
		assertThat(globalBean.getType()).isNotNull().isEqualTo(java.util.Properties.class);
		assertThat(globalBean.getExtensionName()).isEqualTo(CoreConstants.EXTENSIONNAME);
		assertThat(globalBean.getAliases()).isNotNull().isNotEmpty().contains("hybrisProperties");
	}

	@Test
	public void shouldGenerateJSONContainingBeanFromGlobalContext() throws JsonProcessingException
	{
		final Map<String, Object> globalBean = requireSerializedBean("globalHybrisProperties");
		assertThat(globalBean.get(JsonSerializer.BEAN_TYPE_KEY))
				.isNotNull()
				.isEqualTo(java.util.Properties.class.getName());
		assertThat(globalBean.get(JsonSerializer.BEAN_ALIASES_KEY))
				.isNotNull()
				.isInstanceOf(List.class)
				.asList().isNotNull().isNotEmpty().contains("hybrisProperties");
	}

	@Test
	public void shouldGenerateJSONContainingBeanFromApplicationContext() throws JsonProcessingException
	{
		final Map<String, Object> globalBean = requireSerializedBean("applicationHybrisProperties");
		assertThat(globalBean.get(JsonSerializer.BEAN_TYPE_KEY))
				.isNotNull()
				.isEqualTo(java.util.Properties.class.getName());
		assertThat(globalBean.get(JsonSerializer.BEAN_ALIASES_KEY))
				.isNotNull()
				.isInstanceOf(List.class)
				.asList().isNotNull().isNotEmpty().contains("hybrisProperties");
	}

	private Map<String, Object> requireSerializedBean(final String beanName) throws JsonProcessingException
	{
		Objects.requireNonNull(beanName);

		final SpringConfigOverviewProvider givenProvider = givenSpringConfigOverviewProvider();

		final String overview = givenProvider.getCurrentTenantCoreContextOverviewAsJson();
		assertThat(overview).isNotNull().isNotEmpty();

		final Map<String, List<Map<String, Object>>> deserializedOverview = new ObjectMapper().readValue(overview, Map.class);
		assertThat(deserializedOverview).isNotNull().isNotEmpty().containsKey(CoreConstants.EXTENSIONNAME);

		final List<Map<String, Object>> coreBeans = deserializedOverview.get(CoreConstants.EXTENSIONNAME);
		assertThat(coreBeans).isNotNull().isNotEmpty();

		final List<Map<String, Object>> matchingBeans = coreBeans.stream()
		                                                         .filter(b -> beanName.equals(b.get(BEAN_NAME_KEY)))
		                                                         .collect(Collectors.toList());
		assertThat(matchingBeans).hasSize(1).doesNotContainNull();

		return matchingBeans.get(0);
	}

	private BeanInfo requireBean(final String beanName)
	{
		Objects.requireNonNull(beanName);

		final SpringConfigOverviewProvider givenProvider = givenSpringConfigOverviewProvider();

		final Collection<BeanInfo> availableBeans = givenProvider.getAvailableBeansForCurrentTenant();
		assertThat(availableBeans).isNotNull().isNotEmpty().doesNotContainNull();

		final List<BeanInfo> matchingBeans = availableBeans.stream()
		                                                   .filter(b -> beanName.equals(b.getBeanName()))
		                                                   .collect(Collectors.toList());
		assertThat(matchingBeans).hasSize(1).doesNotContainNull();

		return matchingBeans.get(0);
	}

	private SpringConfigOverviewProvider givenSpringConfigOverviewProvider()
	{
		return new SpringConfigOverviewProvider();
	}
}