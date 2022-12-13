/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.scripting.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum ScriptType declared at extension scripting.
 */
public enum ScriptType implements HybrisEnumValue
{
	/**
	 * Generated enum value for ScriptType.GROOVY declared at extension scripting.
	 */
	GROOVY("GROOVY"),
	/**
	 * Generated enum value for ScriptType.BEANSHELL declared at extension scripting.
	 */
	BEANSHELL("BEANSHELL"),
	/**
	 * Generated enum value for ScriptType.JAVASCRIPT declared at extension scripting.
	 */
	JAVASCRIPT("JAVASCRIPT");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "ScriptType";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "ScriptType";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private ScriptType(final String code)
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
