/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer;

import de.hybris.platform.testframework.Transactional;

import org.junit.Ignore;


/**
 * Service layer test with possibility to create sample data. All tests will be executed within transactions.
 */
@Transactional
public class ExtendedServicelayerTransactionalBaseTest extends ExtendedServicelayerBaseTest
{
	// nothing here
}
