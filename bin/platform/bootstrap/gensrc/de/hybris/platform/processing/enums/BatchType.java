/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.processing.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum BatchType declared at extension processing.
 */
public enum BatchType implements HybrisEnumValue
{
	/**
	 * Generated enum value for BatchType.INITIAL declared at extension processing.
	 */
	INITIAL("INITIAL"),
	/**
	 * Generated enum value for BatchType.INPUT declared at extension processing.
	 */
	INPUT("INPUT"),
	/**
	 * Generated enum value for BatchType.RESULT declared at extension processing.
	 */
	RESULT("RESULT");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "BatchType";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "BatchType";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private BatchType(final String code)
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
