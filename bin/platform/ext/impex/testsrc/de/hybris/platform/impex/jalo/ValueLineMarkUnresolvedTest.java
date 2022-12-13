/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.impex.jalo;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.impex.jalo.imp.ValueLine;

import java.util.Collections;

import org.junit.Before;
import org.junit.Test;

@UnitTest
public class ValueLineMarkUnresolvedTest
{
	private static final String REASON = "reason";
	private ValueLine valueLine;

	@Before
	public void setUp()
	{
		valueLine = createTestValueLine();
	}

	@Test
	public void testIfMarkUnresolvedIsNullReasonResistant()
	{
		final boolean unresolvedBefore = valueLine.isUnresolved();
		valueLine.markUnresolved(null);
		final boolean unresolvedAfter = valueLine.isUnresolved();

		assertThat(unresolvedBefore).isEqualTo(FALSE);
		assertThat(unresolvedAfter).isEqualTo(TRUE);
	}

	@Test
	public void testIfMarkUnresolvedIsEmptyReasonResistant()
	{
		final boolean unresolvedBefore = valueLine.isUnresolved();
		valueLine.markUnresolved(EMPTY);
		final boolean unresolvedAfter = valueLine.isUnresolved();

		assertThat(unresolvedBefore).isEqualTo(FALSE);
		assertThat(unresolvedAfter).isEqualTo(TRUE);
	}

	@Test
	public void testIfMarkUnresolvedIsNullReasonResistantWithRecentlyUnresolvedReason()
	{
		markUnresolvedInit(valueLine);
		final boolean unresolvedBefore = valueLine.isUnresolved();
		valueLine.markUnresolved(null);
		final boolean unresolvedAfter = valueLine.isUnresolved();

		assertThat(unresolvedBefore).isEqualTo(TRUE);
		assertThat(unresolvedAfter).isEqualTo(TRUE);
	}

	@Test
	public void testIfMarkUnresolvedIsEmptyReasonResistantWithRecentlyUnresolvedReason()
	{
		markUnresolvedInit(valueLine);
		final boolean unresolvedBefore = valueLine.isUnresolved();
		valueLine.markUnresolved(EMPTY);
		final boolean unresolvedAfter = valueLine.isUnresolved();

		assertThat(unresolvedBefore).isEqualTo(TRUE);
		assertThat(unresolvedAfter).isEqualTo(TRUE);
	}


	private void markUnresolvedInit(final ValueLine valueLine)
	{
		valueLine.markUnresolved(REASON);
	}

	private ValueLine createTestValueLine()
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
		return new ValueLine(null, typeCode, Collections.emptyMap(),
				lineNumber, location);
	}

}
