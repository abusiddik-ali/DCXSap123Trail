/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.impl;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.task.impl.gateways.WorkerStateGateway;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Test;

@UnitTest
public class DefaultWorkerHelperTest
{
	private static final int NUM_OF_WORKERS = 13;
	private static final int RANGE_START = 0;
	private static final int RANGE_END = 103;

	private final WorkerHelper helper = new DefaultWorkerHelper();

	@Test
	public void calculateRangesExclusive()
	{
		assertExclusive(helper.calculateRanges(generateWorkerState(0, NUM_OF_WORKERS, true), RANGE_START, RANGE_END));
	}

	@Test
	public void calculateRangesInclusive()
	{
		assertInclusive(0, helper.calculateRanges(generateWorkerState(0, NUM_OF_WORKERS, false), RANGE_START, RANGE_END));
	}

	@Test
	public void calculateRangesExclusiveAndInclusive()
	{
		final List<WorkerStateGateway.WorkerState> activeWorkers = new ArrayList<>(generateWorkerState(0, NUM_OF_WORKERS, true));
		activeWorkers.addAll(generateWorkerState(NUM_OF_WORKERS, NUM_OF_WORKERS, false));
		final Map<Integer, WorkerStateGateway.WorkerRange> ranges = helper.calculateRanges(activeWorkers, RANGE_START, RANGE_END);
		final Map<Boolean, Map<Integer, WorkerStateGateway.WorkerRange>> rangesIsExclusive =
				activeWorkers.stream()
				             .map(w -> Pair.of(w, ranges.get(w.getNodeId())))
				             .collect(Collectors.partitioningBy(p -> p.getLeft().isExclusiveMode(),
						             Collectors.mapping(p -> Pair.of(p.getLeft().getNodeId(), p.getRight()),
								             Collectors.toMap(Pair::getLeft, Pair::getRight, (v1, v2) -> v2))));
		assertExclusive(rangesIsExclusive.get(true));
		assertInclusive(NUM_OF_WORKERS, rangesIsExclusive.get(false));
	}

	private void assertExclusive(final Map<Integer, WorkerStateGateway.WorkerRange> ranges)
	{
		ranges.forEach((k, v) -> {
			assertThat(v.getStart()).isEqualTo(RANGE_START);
			assertThat(v.getEnd()).isEqualTo(RANGE_END);
		});
	}

	private void assertInclusive(final int offset, final Map<Integer, WorkerStateGateway.WorkerRange> ranges)
	{
		assertThat(ranges.size()).isEqualTo(NUM_OF_WORKERS);
		assertThat(ranges.get(offset).getStart()).isEqualTo(RANGE_START);
		int i = offset + 1;
		final int numOfWorkers = NUM_OF_WORKERS + offset;
		while (i < numOfWorkers)
		{
			assertThat(ranges.get(i - 1).getEnd()).isEqualTo(ranges.get(i).getStart());
			i++;
		}
		assertThat(ranges.get(numOfWorkers - 1).getEnd()).isEqualTo(RANGE_END);
	}

	private List<WorkerStateGateway.WorkerState> generateWorkerState(final int offset, final int number,
	                                                                 final boolean isExclusive)
	{
		final List<WorkerStateGateway.WorkerState> collection = new ArrayList<>(number);
		for (int i = offset, n = number + offset; i < n; i++)
		{
			collection.add(new WorkerStateGateway.WorkerState(i, null, isExclusive, null));
		}
		return collection;
	}

}