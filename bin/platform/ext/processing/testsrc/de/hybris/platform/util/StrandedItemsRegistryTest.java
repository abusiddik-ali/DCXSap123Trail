/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.jalo.user.Title;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.MockitoAnnotations;

@IntegrationTest
public class StrandedItemsRegistryTest extends ServicelayerBaseTest
{

	@Resource
	private ModelService modelService;

	@Captor
	private ArgumentCaptor<Set<PK>> pkSetCaptor;
	@Captor
	private ArgumentCaptor<List<PK>> pkListCaptor;

	@Before
	public void setUp() throws Exception
	{
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void shouldThrowExceptionOnNullHandler()
	{
		assertThatThrownBy(() -> new StrandedItemsRegistry(null)).isInstanceOf(NullPointerException.class);
	}

	@Test
	public void shouldThrowExceptionOnNullPk()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<?> resolutionHandler = spyResolutionHandler();
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		assertThatThrownBy(() -> registry.markStrandedItem(null)).isInstanceOf(NullPointerException.class);
	}

	@Test
	public void shouldThrowExceptionOnNullItem()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<?> resolutionHandler = spyResolutionHandler();
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		assertThatThrownBy(() -> registry.markOnFailure(null, i -> {
		})).isInstanceOf(NullPointerException.class);
	}

	@Test
	public void shouldThrowExceptionOnNullLogic()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<?> resolutionHandler = spyResolutionHandler();
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		final TitleModel item = createTitleModel();

		assertThatThrownBy(() -> registry.markOnFailure(modelService.getSource(item), null)).isInstanceOf(
				NullPointerException.class);
	}

	@Test
	public void shouldNotFailWithEmptyRegistry()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<?> resolutionHandler = spyResolutionHandler();
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		assertThat(registry.getStrandedItems()).isEmpty();

		registry.checkStrandedItems(100);

		assertThat(registry.getStrandedItems()).isEmpty();
	}

	@Test
	public void shouldNotMarkItemOnValidLogicExecution()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<?> resolutionHandler = spyResolutionHandler();
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		final TitleModel itemModel = createTitleModel();
		final Title item = modelService.getSource(itemModel);

		registry.markOnFailure(item, i -> i.setName("titleName"));

		assertThat(registry.getStrandedItems()).isEmpty();
	}


	@Test
	public void shouldMarkItemOnLogicException()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<?> resolutionHandler = spyResolutionHandler();
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		final TitleModel itemModel = createTitleModel();
		final Title item = modelService.getSource(itemModel);

		assertThatThrownBy(() -> registry.markOnFailure(item, i -> {
			throw new IllegalStateException("something went horribly wrong");
		})).isInstanceOf(IllegalStateException.class).withFailMessage("something went horribly wrong");

		assertThat(registry.getStrandedItems()).containsExactly(itemModel.getPk());
	}

	@Test
	public void shouldMarkItemWithPassedPk()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<?> resolutionHandler = spyResolutionHandler();
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		registry.markStrandedItem(PK.fromLong(1));

		assertThat(registry.getStrandedItems()).containsExactly(PK.fromLong(1));
	}

	@Test
	public void shouldPassRegisteredItemsToHandler()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<PK> resolutionHandler = spyResolutionHandler();
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		final Set<PK> pks = new HashSet<>();
		for (int i = 1; i <= 10; i++)
		{
			pks.add(PK.fromLong(i));
		}
		pks.forEach(registry::markStrandedItem);

		assertThat(registry.getStrandedItems()).hasSize(10);
		registry.checkStrandedItems(100);

		assertThat(registry.getStrandedItems()).hasSize(0);

		verify(resolutionHandler).filterItemsToResolve(pkSetCaptor.capture(), any());
		assertThat(pkSetCaptor.getAllValues()).flatExtracting(p -> p).containsExactlyInAnyOrder(pks.toArray(new PK[]{}));

		verify(resolutionHandler).resolveStrandedItems(pkListCaptor.capture(), any());
		assertThat(pkListCaptor.getAllValues()).flatExtracting(p -> p).containsExactlyInAnyOrder(pks.toArray(new PK[]{}));
	}


	@Test
	public void shouldPassRegisteredItemsToHandlerWithNegativeMaxCount()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<PK> resolutionHandler = spyResolutionHandler();
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		final Set<PK> pks = new HashSet<>();
		for (int i = 1; i <= 10; i++)
		{
			pks.add(PK.fromLong(i));
		}
		pks.forEach(registry::markStrandedItem);

		assertThat(registry.getStrandedItems()).hasSize(10);
		registry.checkStrandedItems(-58);

		assertThat(registry.getStrandedItems()).hasSize(0);

		verify(resolutionHandler).filterItemsToResolve(pkSetCaptor.capture(), any());
		assertThat(pkSetCaptor.getAllValues()).flatExtracting(p -> p).containsExactlyInAnyOrder(pks.toArray(new PK[]{}));

		verify(resolutionHandler).resolveStrandedItems(pkListCaptor.capture(), any());
		assertThat(pkListCaptor.getAllValues()).flatExtracting(p -> p).containsExactlyInAnyOrder(pks.toArray(new PK[]{}));
	}

	@Test
	public void shouldPassRegisteredItemsToHandlerWithMaxCountIsZero()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<PK> resolutionHandler = spyResolutionHandler();
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		final Set<PK> pks = new HashSet<>();
		for (int i = 1; i <= 10; i++)
		{
			pks.add(PK.fromLong(i));
		}
		pks.forEach(registry::markStrandedItem);

		assertThat(registry.getStrandedItems()).hasSize(10);
		registry.checkStrandedItems(0);

		assertThat(registry.getStrandedItems()).hasSize(0);

		verify(resolutionHandler).filterItemsToResolve(pkSetCaptor.capture(), any());
		assertThat(pkSetCaptor.getAllValues()).flatExtracting(p -> p).containsExactlyInAnyOrder(pks.toArray(new PK[]{}));

		verify(resolutionHandler).resolveStrandedItems(pkListCaptor.capture(), any());
		assertThat(pkListCaptor.getAllValues()).flatExtracting(p -> p).containsExactlyInAnyOrder(pks.toArray(new PK[]{}));
	}


	@Test
	public void shouldMarkFilteredOutItemsAsChecked()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<PK> resolutionHandler = spyResolutionHandler(pk -> false,
				pk -> true);
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		final Set<PK> pks = new HashSet<>();
		for (int i = 1; i <= 10; i++)
		{
			pks.add(PK.fromLong(i));
		}
		pks.forEach(registry::markStrandedItem);

		assertThat(registry.getStrandedItems()).hasSize(10);
		registry.checkStrandedItems(4);
		assertThat(registry.getStrandedItems()).hasSize(6);
		registry.checkStrandedItems(4);
		assertThat(registry.getStrandedItems()).hasSize(2);
		registry.checkStrandedItems(4);
		assertThat(registry.getStrandedItems()).hasSize(0);

		verify(resolutionHandler, times(3)).filterItemsToResolve(pkSetCaptor.capture(), any());
		assertThat(pkSetCaptor.getAllValues()).extracting(Set::size).containsExactly(4, 4, 2);
		assertThat(pkSetCaptor.getAllValues()).flatExtracting(p -> p)
		                                      .hasSize(10)
		                                      .containsOnlyElementsOf(pks)
		                                      .doesNotHaveDuplicates();

		verify(resolutionHandler, never()).resolveStrandedItems(any(), any());
	}

	@Test
	public void shouldPassRegisteredItemsToHandlerSmallMaxCount()
	{

		final StrandedItemsRegistry.StrandedItemsResolutionHandler<PK> resolutionHandler = spyResolutionHandler();
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		final Set<PK> pks = new HashSet<>();
		for (int i = 1; i <= 10; i++)
		{
			pks.add(PK.fromLong(i));
		}
		pks.forEach(registry::markStrandedItem);

		assertThat(registry.getStrandedItems()).hasSize(10);
		registry.checkStrandedItems(4);
		registry.checkStrandedItems(4);
		registry.checkStrandedItems(4);

		assertThat(registry.getStrandedItems()).hasSize(0);

		verify(resolutionHandler, times(3)).filterItemsToResolve(pkSetCaptor.capture(), any());
		assertThat(pkSetCaptor.getAllValues()).flatExtracting(p -> p)
		                                      .hasSize(10)
		                                      .containsOnlyElementsOf(pks)
		                                      .doesNotHaveDuplicates();

		verify(resolutionHandler, times(3)).resolveStrandedItems(pkListCaptor.capture(), any());
		assertThat(pkListCaptor.getAllValues()).extracting(List::size).containsExactly(4, 4, 2);
		assertThat(pkListCaptor.getAllValues()).flatExtracting(p -> p)
		                                       .hasSize(10)
		                                       .containsOnlyElementsOf(pks)
		                                       .doesNotHaveDuplicates();
	}

	@Test
	public void shouldRetryToResolveNotResolvedItems()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<PK> resolutionHandler = spyResolutionHandler((pk) -> true,
				pk -> false);
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		final PK pk = PK.fromLong(1);
		registry.markStrandedItem(pk);

		assertThat(registry.getStrandedItems()).hasSize(1);
		registry.checkStrandedItems(100);
		registry.checkStrandedItems(100);
		registry.checkStrandedItems(100);

		assertThat(registry.getStrandedItems()).hasSize(1);

		verify(resolutionHandler, times(3)).filterItemsToResolve(pkSetCaptor.capture(), any());
		assertThat(pkSetCaptor.getAllValues()).flatExtracting(p -> p).containsExactly(pk, pk, pk);

		verify(resolutionHandler, times(3)).resolveStrandedItems(pkListCaptor.capture(), any());
		assertThat(pkListCaptor.getAllValues()).flatExtracting(p -> p).containsExactly(pk, pk, pk);
	}


	@Test
	public void shouldRetryToResolveNotResolvedItemsBecauseExceptionHasBeenThrown()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<PK> resolutionHandler = spyResolutionHandler((pk) -> true,
				pk -> {
					throw new RuntimeException("something went horribly wrong");
				});
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		final PK pk = PK.fromLong(1);
		registry.markStrandedItem(pk);

		assertThat(registry.getStrandedItems()).hasSize(1);
		assertThatThrownBy(() -> registry.checkStrandedItems(100)).isInstanceOf(RuntimeException.class)
		                                                          .withFailMessage("something went horribly wrong");
		assertThatThrownBy(() -> registry.checkStrandedItems(100)).isInstanceOf(RuntimeException.class)
		                                                          .withFailMessage("something went horribly wrong");
		assertThatThrownBy(() -> registry.checkStrandedItems(100)).isInstanceOf(RuntimeException.class)
		                                                          .withFailMessage("something went horribly wrong");

		assertThat(registry.getStrandedItems()).hasSize(1);

		verify(resolutionHandler, times(3)).filterItemsToResolve(pkSetCaptor.capture(), any());
		assertThat(pkSetCaptor.getAllValues()).flatExtracting(p -> p).containsExactly(pk, pk, pk);

		verify(resolutionHandler, times(3)).resolveStrandedItems(pkListCaptor.capture(), any());
		assertThat(pkListCaptor.getAllValues()).flatExtracting(p -> p).containsExactly(pk, pk, pk);
	}

	@Test
	public void shouldRetryToResolveNotResolvedItemsBecauseExceptionOnFilteringHasBeenThrown()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<PK> resolutionHandler = spyResolutionHandler((pk) -> {
					throw new RuntimeException("something went horribly wrong");
				},
				pk -> true);
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		final PK pk = PK.fromLong(1);
		registry.markStrandedItem(pk);

		assertThat(registry.getStrandedItems()).hasSize(1);
		assertThatThrownBy(() -> registry.checkStrandedItems(100)).isInstanceOf(RuntimeException.class)
		                                                          .withFailMessage("something went horribly wrong");
		assertThatThrownBy(() -> registry.checkStrandedItems(100)).isInstanceOf(RuntimeException.class)
		                                                          .withFailMessage("something went horribly wrong");
		assertThatThrownBy(() -> registry.checkStrandedItems(100)).isInstanceOf(RuntimeException.class)
		                                                          .withFailMessage("something went horribly wrong");

		assertThat(registry.getStrandedItems()).hasSize(1);

		verify(resolutionHandler, times(3)).filterItemsToResolve(pkSetCaptor.capture(), any());
		assertThat(pkSetCaptor.getAllValues()).flatExtracting(p -> p).containsExactly(pk, pk, pk);

		verify(resolutionHandler, never()).resolveStrandedItems(any(), any());
	}

	@Test
	public void shouldReturnProvidedStrandedItemContext()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<?> resolutionHandler = spyResolutionHandler();
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		final TitleModel itemModel = createTitleModel();
		final Title item = modelService.getSource(itemModel);

		final StrandedItemsRegistry.StrandedItemContext strandedItemContext = new StrandedItemsRegistry.StrandedItemContext();

		assertThatThrownBy(() -> registry.markOnFailure(item, i -> {
			throw new IllegalStateException("something went horribly wrong");
		}, title -> strandedItemContext)).isInstanceOf(IllegalStateException.class)
		                                 .withFailMessage("something went horribly wrong");

		assertThat(registry.getStrandedItems()).containsExactly(itemModel.getPk());
		assertThat(registry.getStrandedItemContext(itemModel.getPk())).isNotEmpty().contains(strandedItemContext);

	}


	@Test
	public void shouldReturnProvidedStrandedItemContextEvenWhenLaterThereIsNoContext()
	{
		final StrandedItemsRegistry.StrandedItemsResolutionHandler<?> resolutionHandler = spyResolutionHandler();
		final StrandedItemsRegistry registry = spyStrandedItemsRegistry(resolutionHandler);

		final TitleModel itemModel = createTitleModel();
		final Title item = modelService.getSource(itemModel);

		final StrandedItemsRegistry.StrandedItemContext strandedItemContext = new StrandedItemsRegistry.StrandedItemContext();

		assertThatThrownBy(() -> registry.markOnFailure(item, i -> {
			throw new IllegalStateException("something went horribly wrong");
		}, title -> strandedItemContext)).isInstanceOf(IllegalStateException.class)
		                                 .withFailMessage("something went horribly wrong");

		assertThatThrownBy(() -> registry.markOnFailure(item, i -> {
			throw new IllegalStateException("something went horribly wrong 2");
		}, title -> null)).isInstanceOf(IllegalStateException.class)
		                  .withFailMessage("something went horribly wrong 2");


		assertThat(registry.getStrandedItems()).containsExactly(itemModel.getPk());
		assertThat(registry.getStrandedItemContext(itemModel.getPk())).isNotEmpty().contains(strandedItemContext);

	}

	private TitleModel createTitleModel()
	{
		final TitleModel item = modelService.create(TitleModel.class);
		item.setCode(UUID.randomUUID().toString());
		modelService.save(item);
		return item;
	}

	private StrandedItemsRegistry spyStrandedItemsRegistry(
			final StrandedItemsRegistry.StrandedItemsResolutionHandler<?> resolutionHandler)
	{
		return spy(new StrandedItemsRegistry(resolutionHandler));
	}

	private TestStrandedItemsResolutionHandler spyResolutionHandler()
	{
		return spy(new TestStrandedItemsResolutionHandler((pk) -> true, pk -> true));
	}

	private TestStrandedItemsResolutionHandler spyResolutionHandler(final Predicate<PK> filterPredicate,
	                                                                final Predicate<PK> resolvePredicate)
	{
		return spy(new TestStrandedItemsResolutionHandler(filterPredicate, resolvePredicate));
	}


	public static class TestStrandedItemsResolutionHandler extends StrandedItemsRegistry.StrandedItemsResolutionHandler<PK>
	{
		final Predicate<PK> filterPredicate;
		final Predicate<PK> resolvePredicate;

		private TestStrandedItemsResolutionHandler(final Predicate<PK> filterPredicate,
		                                           final Predicate<PK> resolvePredicate)
		{
			this.filterPredicate = filterPredicate;
			this.resolvePredicate = resolvePredicate;
		}

		@Override
		public PK getItemPk(final PK item)
		{
			return item;
		}

		@Override
		public List<PK> filterItemsToResolve(final Set<PK> itemsToResolve,
		                                     final StrandedItemsRegistry.StrandedItemContextProvider contextProvider)
		{
			return itemsToResolve.stream()
			                     .filter(filterPredicate)
			                     .collect(Collectors.toList());
		}

		@Override
		public Set<PK> resolveStrandedItems(final List<PK> itemsToResolve,
		                                    final StrandedItemsRegistry.StrandedItemContextProvider contextProvider)
		{
			return itemsToResolve.stream().filter(resolvePredicate).collect(Collectors.toSet());
		}
	}
}
