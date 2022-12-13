/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.validation.interceptors;


import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.validation.model.constraints.ConstraintGroupModel;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@UnitTest
public class ConstraintGroupInterfaceNameValidatorTest extends ServicelayerBaseTest
{
	private ConstraintGroupInterfaceNameValidator validator;
	@Mock
	private ConstraintGroupModel groupModel;
	@Mock
	private InterceptorContext context;

	@Before
	public void setUp()
	{
		MockitoAnnotations.initMocks(this);
		validator = (ConstraintGroupInterfaceNameValidator) Registry.getApplicationContext()
		                                                            .getBean("constraintGroupInterfaceNameValidator");
	}

	@Test
	public void shouldThrowExceptionForInvalidInterfaceMarker()
	{
		for (final String interfaceMarker : getInvalidInterfaceMarkers())
		{
			//given
			Mockito.when(groupModel.getInterfaceName()).thenReturn(interfaceMarker);

			//when, then
			assertThatThrownBy(() -> validator.onValidate(groupModel, context))
					.isInstanceOf(InterceptorException.class)
					.hasMessageContaining("'" + groupModel.getInterfaceName() + "' is not a valid interface name!");
		}
	}

	@Test
	public void shouldNotThrowExceptionForValidInterfaceMarker()
	{
		for (final String interfaceMarker : getValidInterfaceMarkers())
		{
			//given
			Mockito.when(groupModel.getInterfaceName()).thenReturn(interfaceMarker);
			boolean exception = false;

			//when
			try
			{
				validator.onValidate(groupModel, context);
			}
			catch (final Exception e)
			{
				exception = true;
			}

			//then
			assertThat(exception).isFalse();
		}
	}

	private String[] getInvalidInterfaceMarkers()
	{
		return new String[]
				{
						"classname",
						".pack",
						"pack.",
						".pack.classname",
						"pack.classname.",
						"pack.pack.classname.",
						".pack.pack.classname",
						"#pack.classname",
						"pack$",
						"pack,classname",
						"pack.pack,classname",
						"pack.classname,",
						"pack/classname",
						"pack/pack/classname",
						"pack,pack,classname"
				};
	}

	private String[] getValidInterfaceMarkers()
	{
		return new String[]
				{
						"pack.classname",
						"pack.pack.classname",
						"pack.pack.pack.classname",
						"pack.pack.pack.classname",
						"pack.pack.pack.pack.classname"
				};
	}
}