/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.licence;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.jdbcwrapper.HybrisDataSource;
import de.hybris.platform.licence.internal.HybrisLicenceDAO;
import de.hybris.platform.testframework.HybrisJUnit4Test;

import java.util.Date;

import org.junit.Test;

public class HybrisLicenceDAOTest extends HybrisJUnit4Test
{
	private final HybrisLicenceDAO dao = new HybrisLicenceDAO();

	@Test
	public void testGetStartingPointDateForPlatformInstance() throws Exception
	{
		// given
		final HybrisDataSource dataSource = JaloSession.getCurrentSession().getTenant().getDataSource();

		// when
		final Date result = dao.getStartingPointDateForPlatformInstance(dataSource);

		// then
		assertThat(result).isNotNull();
	}
}
