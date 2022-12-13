/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.validation.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum Severity declared at extension validation.
 * <p/>
 * Severities for a constraints.
 */
public enum Severity implements HybrisEnumValue
{
	/**
	 * Generated enum value for Severity.ERROR declared at extension validation.
	 */
	ERROR("ERROR"),
	/**
	 * Generated enum value for Severity.WARN declared at extension validation.
	 */
	WARN("WARN"),
	/**
	 * Generated enum value for Severity.INFO declared at extension validation.
	 */
	INFO("INFO");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "Severity";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "Severity";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private Severity(final String code)
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
