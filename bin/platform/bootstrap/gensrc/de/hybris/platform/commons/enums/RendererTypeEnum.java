/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.commons.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum RendererTypeEnum declared at extension commons.
 */
public enum RendererTypeEnum implements HybrisEnumValue
{
	/**
	 * Generated enum value for RendererTypeEnum.velocity declared at extension commons.
	 */
	VELOCITY("velocity");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "RendererTypeEnum";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "RendererTypeEnum";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private RendererTypeEnum(final String code)
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
