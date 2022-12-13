/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.media.web;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;

import java.util.Optional;

@UnitTest
public class DefaultMediaFilterLogicUnitTest
{
    private TestDefaultMediaFilterLogic mediaFilterLogic;

    @Before
    public void setUp()
    {
        mediaFilterLogic = new TestDefaultMediaFilterLogic();
    }

    @Test
    public void shouldExtractPKFromLocalStrategyLocation()
    {
        //given
        final String location = "testfolderpath/hfd/hcc/8796093317150.html";

        //when
        final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

        //then
        assertThat(dataPKs).isNotEmpty();
        assertThat(dataPKs.get().getLongValueAsString()).isEqualTo("8796093317150");
    }

    @Test
    public void shouldExtractPKFromAzureStrategyLocation()
    {
        //given
        final String location = "hfd/hcc/8796093317150";

        //when
        final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

        //then
        assertThat(dataPKs).isNotEmpty();
        assertThat(dataPKs.get().getLongValueAsString()).isEqualTo("8796093317150");
    }


    @Test
    public void shouldExtractPKFromAnotherAzureStrategyLocation()
    {
        //given
        final String location = "hfd/hcc/8796093317150/test";

        //when
        final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

        //then
        assertThat(dataPKs).isNotEmpty();
        assertThat(dataPKs.get().getLongValueAsString()).isEqualTo("8796093317150");
    }

    @Test
    public void shouldExtractPKFromYetAnotherAzureStrategyLocation()
    {
        //given
        final String location = "hfd/hcc/8796093317150/test.jpg";

        //when
        final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

        //then
        assertThat(dataPKs).isNotEmpty();
        assertThat(dataPKs.get().getLongValueAsString()).isEqualTo("8796093317150");
    }

    @Test
    public void shouldExtractPKFromGridFsStrategyLocation()
    {
        //given
        final String location = "8796093317150";

        //when
        final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

        //then
        assertThat(dataPKs).isNotEmpty();
        assertThat(dataPKs.get().getLongValueAsString()).isEqualTo("8796093317150");
    }

    @Test
    public void shouldExtractPKFromAnotherGridFsStrategyLocation()
    {
        //given
        final String location = "8796093317150/test";

        //when
        final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

        //then
        assertThat(dataPKs).isNotEmpty();
        assertThat(dataPKs.get().getLongValueAsString()).isEqualTo("8796093317150");
    }

    @Test
    public void shouldExtractPKFromYetAnotherGridFsStrategyLocation()
    {
        //given
        final String location = "8796093317150/test.21.txt";

        //when
        final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

        //then
        assertThat(dataPKs).isNotEmpty();
        assertThat(dataPKs.get().getLongValueAsString()).isEqualTo("8796093317150");
    }

    @Test
    public void shouldExtractPKWhenManyEqualPKsInLocation()
    {
        //given
        final String location = "/medias/sys_master/root/8796093317150/8796093317150.txt";

        //when
        final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

        //then
        //then
        assertThat(dataPKs).isNotEmpty();
        assertThat(dataPKs.get().getLongValueAsString()).isEqualTo("8796093317150");
    }

    @Test
    public void shouldExtractPKWhenCorrectAndIncorrectPKsInLocation()
    {
        //given
        final String location = "/medias/sys_master/root/879671653/8796093317150.txt";

        //when
        final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

        //then
        //then
        assertThat(dataPKs).isNotEmpty();
        assertThat(dataPKs.get().getLongValueAsString()).isEqualTo("8796093317150");
    }

	@Test
	public void shouldExtractMaxLong()
	{
		//given
		final String location = "testfolderpath/hfd/9223372036854775807/image.jpeg";

		//when
		final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

		//then
		assertThat(dataPKs).isNotEmpty();
		assertThat(dataPKs.get().getLongValueAsString()).isEqualTo("9223372036854775807");
	}

	@Test
	public void shouldNotExtractMaxLongPlus1()
	{
		//given
		final String location = "testfolderpath/hfd/9223372036854775808/image.jpeg";

		//when
		final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

		//then
		assertThat(dataPKs).isEmpty();
	}

	@Test
	public void shouldNotExtractMaxLongPlusOneDigit()
	{
		//given
		final String location = "testfolderpath/hfd/92233720368547758071/image.jpeg";

		//when
		final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

		//then
		assertThat(dataPKs).isEmpty();
	}

	@Test
	public void shouldNotExtractPKWhenShortAndLongCorrectPKsInLocation()
	{
		//given
		final String location = "/medias/sys_master/root/8796093317150/8796093317150446453.txt";

		//when
		final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

		//then
		assertThat(dataPKs).isEmpty();
	}

    @Test
    public void shouldNotExtractPKFromBrokenLocation()
    {
        //given
        final String location = "testfolderpath/hfd/hcc/879671653.html";

        //when
        final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

        //then
        assertThat(dataPKs).isEmpty();
    }

    @Test
    public void shouldNotExtractPKFromEmptyLocation()
    {
        //given
        final String location = "";

        //when
        final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

        //then
        assertThat(dataPKs).isEmpty();
    }

    @Test
    public void shouldNotExtractWhenManyUnequalPKsInLocation()
    {
        //given
        final String location = "/medias/sys_master/root/8796093317150/8796093349918.txt";

        //when
        final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

        //then
        assertThat(dataPKs).isEmpty();
    }

    @Test
    public void shouldNotExtractWhenOnlyLiteralsInLocation()
    {
        //given
        final String location = "testfolderpath/hfd/hcc/name_of_the_file_I_really_want.html";

        //when
        final Optional<PK> dataPKs = mediaFilterLogic.extractDataPKFromLocation(location);

        //then
        assertThat(dataPKs).isEmpty();
    }
}
