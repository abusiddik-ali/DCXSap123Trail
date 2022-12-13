/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.validation.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum ValidatorLanguage declared at extension validation.
 * <p/>
 * Language of dynamic validation.
 */
public enum ValidatorLanguage implements HybrisEnumValue
{
	/**
	 * Generated enum value for ValidatorLanguage.BEANSHELL declared at extension validation.
	 */
	BEANSHELL("BEANSHELL");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "ValidatorLanguage";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "ValidatorLanguage";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private ValidatorLanguage(final String code)
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
