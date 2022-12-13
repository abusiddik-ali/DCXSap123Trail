/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util.localization.jdbc;

import de.hybris.platform.util.localization.jdbc.rows.LocalizableAttributeRow;

import org.apache.commons.lang3.StringUtils;


/**
 * Filters out localization of attributes of {@link JdbcBasedTypeSystemLocalizationIntegrationTest#TEST_TYPECODE}
 */
public class TestTypeCodeLocalizableAttributeRowFilterStub implements LocalizableAttributeRowFilter
{

	@Override
	public boolean filter(final LocalizableAttributeRow row)
	{
		return !StringUtils.equalsIgnoreCase(row.getOwnerCode(), JdbcBasedTypeSystemLocalizationIntegrationTest.TEST_TYPECODE);
	}
}