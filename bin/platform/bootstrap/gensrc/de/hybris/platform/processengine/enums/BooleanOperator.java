/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.processengine.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum BooleanOperator declared at extension processing.
 */
public enum BooleanOperator implements HybrisEnumValue
{
	/**
	 * Generated enum value for BooleanOperator.AND declared at extension processing.
	 */
	AND("AND"),
	/**
	 * Generated enum value for BooleanOperator.OR declared at extension processing.
	 */
	OR("OR");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "BooleanOperator";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "BooleanOperator";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private BooleanOperator(final String code)
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
