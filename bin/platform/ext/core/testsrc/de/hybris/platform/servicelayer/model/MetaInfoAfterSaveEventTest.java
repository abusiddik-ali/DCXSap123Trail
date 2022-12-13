/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.model;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.hamcrest.Matchers.instanceOf;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.atMost;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Constants;
import de.hybris.platform.core.PK;
import de.hybris.platform.jalo.numberseries.NumberSeriesManager;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.exceptions.ModelLoadingException;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.tx.AfterSaveEvent;
import de.hybris.platform.tx.AfterSaveListener;
import de.hybris.platform.tx.AfterSaveListenerRegistry;
import de.hybris.platform.tx.DefaultAfterSaveListenerRegistry;

import java.time.Duration;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.MockitoAnnotations;

@IntegrationTest
public class MetaInfoAfterSaveEventTest extends ServicelayerBaseTest
{
	private final static Duration LISTENER_WAIT_DURATION = Duration.ofSeconds(20);
	private final Set<AfterSaveListener> listeners = new HashSet<>();
	private final PropertyConfigSwitcher ignoredTypes = new PropertyConfigSwitcher("core.aftersave.ignoredtypes");
	@Captor
	ArgumentCaptor<Collection<AfterSaveEvent>> afterEventCollection;
	@Resource
	private AfterSaveListenerRegistry afterSaveListenerRegistry;
	@Resource
	private ModelService modelService;

	@Before
	public void setUp()
	{
		Assume.assumeThat(afterSaveListenerRegistry, instanceOf(DefaultAfterSaveListenerRegistry.class));
		MockitoAnnotations.initMocks(this);
	}

	@After
	public void tearDown()
	{
		ignoredTypes.switchBackToDefault();
		if (afterSaveListenerRegistry instanceof DefaultAfterSaveListenerRegistry)
		{
			listeners.forEach(l -> ((DefaultAfterSaveListenerRegistry) afterSaveListenerRegistry).removeListener(l));
		}
	}

	@Test
	public void shouldNotPublishEventsForMetaInformationTypeWhenPropertyHasDefaultValue() throws InterruptedException
	{
		final AfterSaveListener listener = createListener();
		((DefaultAfterSaveListenerRegistry) afterSaveListenerRegistry).addListener(listener);

		NumberSeriesManager.getInstance().setDigits(UUID.randomUUID().toString(), 17);

		Thread.sleep(LISTENER_WAIT_DURATION.toMillis());

		// atMost here is only to not fail if there is no invocation of this method
		verify(listener, atMost(Integer.MAX_VALUE)).afterSave(afterEventCollection.capture());

		final Stream<AfterSaveEvent> events = afterEventCollection.getAllValues().stream().flatMap(Collection::stream);
		assertThat(events).extracting(AfterSaveEvent::getPk)
		                  .extracting(PK::getTypeCode)
		                  .doesNotContain(Constants.TC.MetaInformation);

	}

	private AfterSaveListener createListener()
	{
		final AfterSaveListener mock = mock(AfterSaveListener.class);
		listeners.add(mock);
		return mock;
	}

	@Test
	public void shouldPublishEventsForMetaInformationTypeWhenPropertyDoesntContainTheTypeCode() throws InterruptedException
	{
		ignoredTypes.switchToValue("30,222");
		final AfterSaveListener listener = createListener();
		((DefaultAfterSaveListenerRegistry) afterSaveListenerRegistry).addListener(listener);

		NumberSeriesManager.getInstance().setDigits(UUID.randomUUID().toString(), 17);

		Thread.sleep(LISTENER_WAIT_DURATION.toMillis());

		verify(listener, atLeastOnce()).afterSave(afterEventCollection.capture());

		final Stream<AfterSaveEvent> events = afterEventCollection.getAllValues().stream().flatMap(Collection::stream);
		assertThat(events).extracting(AfterSaveEvent::getPk)
		                  .extracting(PK::getTypeCode)
		                  .contains(Constants.TC.MetaInformation);
	}

	@Test
	public void shouldPublishEventsForMetaInformationTypeWhenPropertyIsEmpty() throws InterruptedException
	{
		ignoredTypes.switchToValue("");
		final AfterSaveListener listener = createListener();
		((DefaultAfterSaveListenerRegistry) afterSaveListenerRegistry).addListener(listener);

		NumberSeriesManager.getInstance().setDigits(UUID.randomUUID().toString(), 17);

		Thread.sleep(LISTENER_WAIT_DURATION.toMillis());

		verify(listener, atLeastOnce()).afterSave(afterEventCollection.capture());

		final Stream<AfterSaveEvent> events = afterEventCollection.getAllValues().stream().flatMap(Collection::stream);
		assertThat(events).extracting(AfterSaveEvent::getPk)
		                  .extracting(PK::getTypeCode)
		                  .contains(Constants.TC.MetaInformation);
	}


	@Test
	public void shouldThrowValidExceptionWhenLoadingMetaInformationFromModelService()
	{

		assertThatThrownBy(() -> modelService.get(PK.createUUIDPK(Constants.TC.MetaInformation)))
				.isInstanceOf(ModelLoadingException.class);

	}
}
