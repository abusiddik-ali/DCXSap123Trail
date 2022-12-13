/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.interceptor.impl;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.media.MediaContextModel;
import de.hybris.platform.core.model.media.MediaFormatMappingModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;


/**
 * Unit test covering a {@link MediaContextValidator} implementation.
 */
@UnitTest
public class MediaContextValidatorTest
{
	@Mock
	private InterceptorContext ctx;

	private MediaContextValidator validator;

	@Before
	public void prepare()
	{
		validator = new MediaContextValidator();
		MockitoAnnotations.initMocks(this);
	}

	@Test
	public void testNullModel() throws InterceptorException
	{
		validator.onValidate(null, ctx);
	}

	@Test
	public void testEmptyMappings() throws InterceptorException
	{

		final MediaContextModel model = Mockito.spy(new MediaContextModel());
		validator.onValidate(model, ctx);
		Mockito.verify(model, Mockito.times(1)).getMappings();
	}

	@Test
	public void testNonDuplicatedMappings() throws InterceptorException
	{

		final MediaFormatMappingModel mappingOne = new MediaFormatMappingModel();
		mappingOne.setSource(new MediaFormatModel());
		final MediaFormatMappingModel mappingTwo = new MediaFormatMappingModel();
		mappingTwo.setSource(new MediaFormatModel());
		final MediaFormatMappingModel mappingThree = new MediaFormatMappingModel();
		mappingThree.setSource(new MediaFormatModel());

		final MediaContextModel model = Mockito.spy(new MediaContextModel());

		model.setMappings(Arrays.asList(mappingOne, mappingTwo, mappingThree));

		validator.onValidate(model, ctx);

		Mockito.verify(model, Mockito.times(3)).getMappings();
	}

	@Test(expected = InterceptorException.class)
	public void testDuplicatedMappings() throws InterceptorException
	{

		final MediaFormatMappingModel mappingOne = new MediaFormatMappingModel();
		mappingOne.setSource(new MediaFormatModel());
		final MediaFormatMappingModel mappingTwo = new MediaFormatMappingModel();
		mappingTwo.setSource(new MediaFormatModel());
		final MediaFormatMappingModel mappingThree = mappingOne;

		final MediaContextModel model = Mockito.spy(new MediaContextModel());

		model.setMappings(Arrays.asList(mappingOne, mappingTwo, mappingThree));

		validator.onValidate(model, ctx);

		Mockito.verify(model, Mockito.times(2)).getMappings();
	}
}
