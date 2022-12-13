/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum ActionType declared at extension core.
 */
public enum ActionType implements HybrisEnumValue
{
	/**
	 * Generated enum value for ActionType.PLAIN declared at extension core.
	 */
	PLAIN("PLAIN"),
	/**
	 * Generated enum value for ActionType.TASK declared at extension processing.
	 */
	TASK("TASK"),
	/**
	 * Generated enum value for ActionType.PROCESS declared at extension processing.
	 */
	PROCESS("PROCESS");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "ActionType";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "ActionType";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private ActionType(final String code)
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
