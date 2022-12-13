/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.validation.pojos;

import java.util.Date;


public class PojoTwo extends PojoOne
{
	private Date pojoTwoPrivate;
	public Date pojoTwoPublic;

	public Date getPojoTwoPrivate()
	{
		return pojoTwoPrivate;
	}

	public void setPojoTwoPrivate(final Date pojoTwoPrivate)
	{
		this.pojoTwoPrivate = pojoTwoPrivate;
	}
}
