/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.media.impl;

import de.hybris.bootstrap.annotations.IntegrationTest;

@IntegrationTest
public class LocalMediaStoragePrettyUrlIntegrationTest extends AbstractMediaStoragePrettyUrlIntegrationTest
{
	@Override
	protected String getTestFolderQualifier()
	{
		return "localstorageprettyurltesting";
	}
}
