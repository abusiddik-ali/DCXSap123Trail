/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.validation.pojos;

import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.validation.enums.Severity;
import de.hybris.platform.validation.model.constraints.AttributeConstraintModel;
import de.hybris.platform.validation.model.constraints.jsr303.SizeConstraintModel;

import java.util.Arrays;
import java.util.Collection;

import org.apache.log4j.Logger;
import org.hibernate.validator.constraints.Length;


public class SamplePojo
{
	private static final Logger LOG = Logger.getLogger(SamplePojo.class);

	@Length(min = 5, max = 10)
	private String stringField;

	public String getStringField()
	{
		return stringField;
	}

	public void setStringField(final String stringField)
	{
		this.stringField = stringField;
	}

	public static Collection<? extends AttributeConstraintModel> buildConstraintModel(final ModelService service)
	{

		final SizeConstraintModel size = service.create(SizeConstraintModel.class);
		size.setId(SamplePojo.class.getName() + "-" + SizeConstraintModel._TYPECODE);
		size.setTarget(SamplePojo.class);
		size.setQualifier("stringField");
		size.setMin(Long.valueOf(5L));
		size.setMax(Long.valueOf(10L));
		size.setSeverity(Severity.ERROR);

		return Arrays.asList(size);
	}
}
