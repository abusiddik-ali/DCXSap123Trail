/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer;


/**
 * The interface used to lookup bean used to run import during tests. The purpose of providing this interface is to
 * enable using impex in service layer tests. Default implementation is
 * {@link de.hybris.platform.servicelayer.impex.DefaultTestImportCsvUtil} and it is configured in
 * servicelayer-spring.xml
 * <p>
 * The usage is to lookup default implementation using Registry.getApplicationContext().getBean("testImportCsvUtil");<br>
 * See {@link de.hybris.platform.servicelayer.type.daos.impl.DefaultTypeDaoTest}
 */
public interface TestImportCsvUtil
{
	void importCsv(final String csvFile, final String encoding);
}
