/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.processing.distributed.simple;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.processing.distributed.defaultimpl.DistributedProcessHandler;
import de.hybris.platform.processing.distributed.simple.context.SimpleProcessCreationContext;
import de.hybris.platform.processing.distributed.simple.data.QueryBasedCreationData;
import de.hybris.platform.processing.model.BatchModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import javax.annotation.Resource;

import org.junit.Test;


public class SimpleProcessCreationContextTest extends ServicelayerBaseTest
{
	@Resource
	private ModelService modelService;
	@Resource
	private FlexibleSearchService flexibleSearchService;

	@Test
	public void shouldCreate_0_batchesFor_nonResultingQueryWithBatchSize_100() throws Exception
	{
		// given (no titles are created query will return empty result set)
		final QueryBasedCreationData testProcessData = QueryBasedCreationData.builder()
		                                                                     .withQuery("SELECT {PK} FROM {Title}")
		                                                                     .withHandlerId(
				                                                                     "testSimpleDistributedProcessHandler") //
		                                                                     .withProcessId("TEST_PROCESS") //
		                                                                     .withBatchSize(100) //
		                                                                     .build();
		final SimpleProcessCreationContext ctx = new SimpleProcessCreationContext(modelService, testProcessData);

		// when
		final Stream<DistributedProcessHandler.ModelWithDependencies<BatchModel>> batches = ctx.initialBatches();

		// then
		assertThat(batches).isNotNull();
		final List<BatchModel> collect = batches.map(DistributedProcessHandler.ModelWithDependencies::getModel)
		                                        .collect(Collectors.toList());
		assertThat(collect).hasSize(0);
	}

	@Test
	public void shouldCreate_1_batchFor_exact_100_ItemsQueryWithBatchSize_100() throws Exception
	{
		// given
		createNumOfTitles(100);
		final QueryBasedCreationData testProcessData = QueryBasedCreationData.builder()
		                                                                     .withQuery("SELECT {PK} FROM {Title}")
		                                                                     .withHandlerId(
				                                                                     "testSimpleDistributedProcessHandler") //
		                                                                     .withProcessId("TEST_PROCESS") //
		                                                                     .withBatchSize(100) //
		                                                                     .build();
		final SimpleProcessCreationContext ctx = new SimpleProcessCreationContext(modelService, testProcessData);

		// when
		final Stream<DistributedProcessHandler.ModelWithDependencies<BatchModel>> batches = ctx.initialBatches();

		// then
		assertThat(batches).isNotNull();
		final List<BatchModel> collect = batches.map(DistributedProcessHandler.ModelWithDependencies::getModel)
		                                        .collect(Collectors.toList());
		assertThat(collect).hasSize(1);
	}

	@Test
	public void shouldCreate_1_batchFor_lessThan_100_ItemsQueryWithBatchSize_100() throws Exception
	{
		// given
		createNumOfTitles(50);
		final QueryBasedCreationData testProcessData = QueryBasedCreationData.builder()
		                                                                     .withQuery("SELECT {PK} FROM {Title}")
		                                                                     .withHandlerId(
				                                                                     "testSimpleDistributedProcessHandler") //
		                                                                     .withProcessId("TEST_PROCESS") //
		                                                                     .withBatchSize(100) //
		                                                                     .build();
		final SimpleProcessCreationContext ctx = new SimpleProcessCreationContext(modelService, testProcessData);

		// when
		final Stream<DistributedProcessHandler.ModelWithDependencies<BatchModel>> batches = ctx.initialBatches();

		// then
		assertThat(batches).isNotNull();
		final List<BatchModel> collect = batches.map(DistributedProcessHandler.ModelWithDependencies::getModel)
		                                        .collect(Collectors.toList());
		assertThat(collect).hasSize(1);
	}

	@Test
	public void shouldCreate_10_batchesFor_1000_ItemsQueryWithBatchSize_100() throws Exception
	{
		// given
		createNumOfTitles(1000);
		final QueryBasedCreationData testProcessData = QueryBasedCreationData.builder()
		                                                                     .withQuery("SELECT {PK} FROM {Title}")
		                                                                     .withHandlerId(
				                                                                     "testSimpleDistributedProcessHandler") //
		                                                                     .withProcessId("TEST_PROCESS") //
		                                                                     .withBatchSize(100) //
		                                                                     .build();
		final SimpleProcessCreationContext ctx = new SimpleProcessCreationContext(modelService, testProcessData);

		// when
		final Stream<DistributedProcessHandler.ModelWithDependencies<BatchModel>> batches = ctx.initialBatches();

		// then
		assertThat(batches).isNotNull();
		final List<BatchModel> collect = batches.map(DistributedProcessHandler.ModelWithDependencies::getModel)
		                                        .collect(Collectors.toList());
		assertThat(collect).hasSize(10);
	}

	private void createNumOfTitles(final int num)
	{
		IntStream.range(0, num).forEach(this::createTitle);
	}

	private void createTitle(final int i)
	{
		final TitleModel title = modelService.create(TitleModel.class);
		title.setCode(i + "_" + UUID.randomUUID().toString());

		modelService.save(title);
	}

}
