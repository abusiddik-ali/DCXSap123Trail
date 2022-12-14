/*
 * ----------------------------------------------------------------
 * --- WARNING: THIS FILE IS GENERATED AND WILL BE OVERWRITTEN!
 * --- Generated at 14-Dec-2022, 8:42:45 PM
 * ----------------------------------------------------------------
 *
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test.beans;

import java.io.Serializable;


import java.util.Objects;
public  class TestBean  implements Serializable 

{

	/** Default serialVersionUID value. */
 
	private static final long serialVersionUID = 1L;

	/** <i>Generated property</i> for <code>TestBean.stringProperty</code> property defined at extension <code>core</code>. */
	
	private String stringProperty;

	/** <i>Generated property</i> for <code>TestBean.integerProperty</code> property defined at extension <code>core</code>. */
	
	private Integer integerProperty;

	/** <i>Generated property</i> for <code>TestBean.booleanProperty</code> property defined at extension <code>core</code>. */
	
	private boolean booleanProperty;

	/** <i>Generated property</i> for <code>TestBean.nativeProperty</code> property defined at extension <code>core</code>. */
	
	private int nativeProperty;

	/** <i>Generated property</i> for <code>TestBean.equalsA</code> property defined at extension <code>core</code>. */
	
	private String equalsA;

	/** <i>Generated property</i> for <code>TestBean.equalsB</code> property defined at extension <code>core</code>. */
	
	private Integer equalsB;

	/** <i>Generated property</i> for <code>TestBean.equalsC</code> property defined at extension <code>core</code>. */
	
	private Boolean equalsC;
	
	public TestBean()
	{
		// default constructor
	}
	
	public void setStringProperty(final String stringProperty)
	{
		this.stringProperty = stringProperty;
	}

	public String getStringProperty() 
	{
		return stringProperty;
	}
	
	public void setIntegerProperty(final Integer integerProperty)
	{
		this.integerProperty = integerProperty;
	}

	public Integer getIntegerProperty() 
	{
		return integerProperty;
	}
	
	public void setBooleanProperty(final boolean booleanProperty)
	{
		this.booleanProperty = booleanProperty;
	}

	public boolean isBooleanProperty() 
	{
		return booleanProperty;
	}
	
	public void setNativeProperty(final int nativeProperty)
	{
		this.nativeProperty = nativeProperty;
	}

	public int getNativeProperty() 
	{
		return nativeProperty;
	}
	
	public void setEqualsA(final String equalsA)
	{
		this.equalsA = equalsA;
	}

	public String getEqualsA() 
	{
		return equalsA;
	}
	
	public void setEqualsB(final Integer equalsB)
	{
		this.equalsB = equalsB;
	}

	public Integer getEqualsB() 
	{
		return equalsB;
	}
	
	public void setEqualsC(final Boolean equalsC)
	{
		this.equalsC = equalsC;
	}

	public Boolean getEqualsC() 
	{
		return equalsC;
	}
	

	@Override
	public boolean equals(final Object o)
	{
		if (o == null) return false;
		if (o == this) return true;

        if (getClass() != o.getClass()) return false;

		final TestBean other = (TestBean) o;
		return Objects.equals(getEqualsA(), other.getEqualsA())

			&& Objects.equals(getEqualsB(), other.getEqualsB())

			&& Objects.equals(getEqualsC(), other.getEqualsC());


    }

	@Override
	public int hashCode()
	{
		int result = 1;
		Object attribute;

		attribute = equalsA;
		result = 31 * result + (attribute == null ? 0 : attribute.hashCode());
		attribute = equalsB;
		result = 31 * result + (attribute == null ? 0 : attribute.hashCode());
		attribute = equalsC;
		result = 31 * result + (attribute == null ? 0 : attribute.hashCode());

		return result;
	}
}