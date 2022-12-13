/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.impex.jalo.imp;

import static java.lang.Boolean.TRUE;
import static org.apache.commons.lang.StringUtils.EMPTY;
import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;

import java.util.Collections;

import org.junit.Before;
import org.junit.Test;

@UnitTest
public class MultiThreadedImpExImportReaderProcessPendingResultTest extends ServicelayerBaseTest
{

	private static final String EXCEPTION_MSG = "Exception!";
	private static final String UNRESOLVED_REASON = "Reason!";
	private MultiThreadedImpExImportReader importReader;

	@Before
	public void setUp()
	{
		importReader = MultiThreadedImpExImportReaderTest.createMTIIRTest();
	}

	@Test
	public void testProcessPendingResultWithNoErrorAgainstMarkUnresolved()
	{
		final ImpExWorkerResult impExWorkerResult = createImpExWorkerResultAndReturnRelevantLine();

		final boolean pendingResult = importReader.processPendingResult(impExWorkerResult);

		assertThat(pendingResult).isEqualTo(TRUE);
		assertThat(getUnresolvedReason(impExWorkerResult)).isEqualTo(UNRESOLVED_REASON);
	}

	private String getUnresolvedReason(final ImpExWorkerResult impExWorkerResult)
	{
		return impExWorkerResult.getLine().getUnresolvedReason();
	}

	@Test
	public void testProcessPendingResultWithMsgErrorAgainstMarkUnresolved()
	{
		final ImpExWorkerResult impExWorkerResult = createImpExWorkerResultAndReturnRelevantLine(withError());

		final boolean pendingResult = importReader.processPendingResult(impExWorkerResult);

		assertThat(pendingResult).isEqualTo(TRUE);
		assertThat(getUnresolvedReason(impExWorkerResult)).contains(EXCEPTION_MSG);

	}

	@Test
	public void testProcessPendingResultWithNullMsgErrorAgainstMarkUnresolved()
	{
		final ImpExWorkerResult impExWorkerResult = createImpExWorkerResultAndReturnRelevantLine(withNullMsgError());

		final boolean pendingResult = importReader.processPendingResult(impExWorkerResult);

		assertThat(pendingResult).isEqualTo(TRUE);
		assertThat(getUnresolvedReason(impExWorkerResult)).contains(UNRESOLVED_REASON);
	}

	private Exception withError()
	{
		return new Exception(EXCEPTION_MSG);
	}

	private Exception withNullMsgError()
	{
		final String nullMsg = null;
		return new Exception(nullMsg);
	}

	private ImpExWorkerResult createImpExWorkerResultAndReturnRelevantLine(final Exception error)
	{
		return new ImpExWorkerResult(null, createTestValueLineWithUnresolvedReason(), null, error);
	}

	private ImpExWorkerResult createImpExWorkerResultAndReturnRelevantLine()
	{
		return createImpExWorkerResultAndReturnRelevantLine(null);
	}


	private ValueLine createTestValueLineWithUnresolvedReason()
	{
		final String location = "location";
		final int lineNumber = 123;
		final String typeCode = "typeCode" +
				"," +
				"1234567890" +
				"," +
				"," +
				"9876543210" +
				",";
		final ValueLine valueLine = new ValueLine(null, typeCode, Collections.emptyMap(),
				lineNumber, location);
		//just for UnresolvedReason
		valueLine.markUnrecoverable(UNRESOLVED_REASON);
		return valueLine;
	}

	private static class MultiThreadedImpExImportReaderTest extends MultiThreadedImpExImportReader
	{

		public MultiThreadedImpExImportReaderTest(final String lines)
		{
			super(lines);
		}

		public static MultiThreadedImpExImportReaderTest createMTIIRTest()
		{
			return new MultiThreadedImpExImportReaderTest(EMPTY);
		}

		@Override
		protected boolean processPendingResult(final ImpExWorkerResult result)
		{
			return super.processPendingResult(result);
		}

		@Override
		protected boolean mustMarkLineAsUnresolved(final ImpExWorkerResult result, final ValueLine line)
		{
			return true;
		}
	}
}
