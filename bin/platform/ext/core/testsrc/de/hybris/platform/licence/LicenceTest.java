/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.licence;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.mock;

import java.util.Properties;

import org.junit.Test;

public class LicenceTest
{

	@Test
	public void licenceShouldBeDemoLicenceWhenItsIdStartsFromCoupleOfZerosFollowingByOneAndDash() throws Exception
	{
		// given
		final Licence licence = getLicenceStubWithEulaAndId("2.0", "000001-02");

		// when
		final boolean result = licence.isDemoOrDevelopLicence();

		// then
		assertThat(result).isTrue();
	}

	@Test
	public void licenceShouldBeDemoLicenceWhenItsIdStartsFromWithEvenMoreOfZerosFollowingByOneAndDash() throws Exception
	{
		// given
		final Licence licence = getLicenceStubWithEulaAndId("2.0", "00000000001-02");

		// when
		final boolean result = licence.isDemoOrDevelopLicence();

		// then
		assertThat(result).isTrue();
	}

	@Test
	public void licenceShouldNotBeDemoLicenceWhenItsIdStartsFromSomethingOtherThanZerosFollowedByOneAndDash() throws Exception
	{
		// given
		final Licence licence = getLicenceStubWithEulaAndId("2.0", "0000200010001-02");

		// when
		final boolean result = licence.isDemoOrDevelopLicence();

		// then
		assertThat(result).isFalse();
	}

	@Test
	public void masterServerShouldBeEnabledWhenEulaIsGreaterThanTwoPointZero() throws Exception
	{
		// given
		final Licence licence = getLicenceStubWithEulaAndId("2.1", "000001-02");

		// when
		final boolean result = licence.isMasterServerEnabled();

		// then
		assertThat(result).isTrue();
	}

	@Test
	public void masterServerShouldBeEnabledWhenEulaIsEqualTwoPointZero() throws Exception
	{
		// given
		final Licence licence = getLicenceStubWithEulaAndId("2.0", "000001-02");

		// when
		final boolean result = licence.isMasterServerEnabled();

		// then
		assertThat(result).isTrue();
	}

	@Test
	public void masterServerShouldBeDisabledWhenEulaIsLowerThanTwoPointZero() throws Exception
	{
		// given
		final Licence licence = getLicenceStubWithEulaAndId("1.9", "000001-02");

		// when
		final boolean result = licence.isMasterServerEnabled();

		// then
		assertThat(result).isFalse();
	}

	@Test
	public void masterServerShouldBeDisabledWhenLicenceDoesNotHaveEulaInProps() throws Exception
	{
		// given
		final Licence licence = getLicenceStubWithEulaAndId(null, "000001-02");

		// when
		final boolean result = licence.isMasterServerEnabled();

		// then
		assertThat(result).isFalse();
	}

	@Test
	public void masterServerShouldBeDisabledWhenLicenceHasEmptyEulaInProps() throws Exception
	{
		// given
		final Licence licence = getLicenceStubWithEulaAndId(" ", "000001-02");

		// when
		final boolean result = licence.isMasterServerEnabled();

		// then
		assertThat(result).isFalse();
	}

	private Licence getLicenceStubWithEulaAndId(final String eulaVer, final String id)
	{
		final Properties props = mock(Properties.class);
		given(props.getProperty("licence.eulaversion")).willReturn(eulaVer);
		given(props.getProperty("licence.id")).willReturn(id);

		return new Licence()
		{
			@Override
			public Properties getLicenceProperties()
			{
				return props;
			}

			@Override
			public byte[] getSignature()
			{
				return new byte[0];
			}
		};
	}
}
