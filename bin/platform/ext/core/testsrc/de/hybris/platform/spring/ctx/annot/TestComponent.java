/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.spring.ctx.annot;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;


@Controller
public class TestComponent
{

	@Autowired(required = true)
	private TestSubComponent testBean;

	/**
	 * @return the testBean
	 */
	public TestSubComponent getTestBean()
	{
		return testBean;
	}

}
