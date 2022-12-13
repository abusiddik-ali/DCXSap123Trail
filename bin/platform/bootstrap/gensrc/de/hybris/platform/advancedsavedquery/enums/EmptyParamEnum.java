/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.advancedsavedquery.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum EmptyParamEnum declared at extension advancedsavedquery.
 */
public enum EmptyParamEnum implements HybrisEnumValue
{
	/**
	 * Generated enum value for EmptyParamEnum.ignore declared at extension advancedsavedquery.
	 */
	IGNORE("ignore"),
	/**
	 * Generated enum value for EmptyParamEnum.trimAndIgnore declared at extension advancedsavedquery.
	 */
	TRIMANDIGNORE("trimAndIgnore"),
	/**
	 * Generated enum value for EmptyParamEnum.asitis declared at extension advancedsavedquery.
	 */
	ASITIS("asitis");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "EmptyParamEnum";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "EmptyParamEnum";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private EmptyParamEnum(final String code)
	{
		this.code = code.intern();
	}
	
	
	/**
	 * Gets the code of this enum value.
	 *  
	 * @return code of value
	 */
	@Override
	public String getCode()
	{
		return this.code;
	}
	
	/**
	 * Gets the type this enum value belongs to.
	 *  
	 * @return code of type
	 */
	@Override
	public String getType()
	{
		return SIMPLE_CLASSNAME;
	}
	
}
