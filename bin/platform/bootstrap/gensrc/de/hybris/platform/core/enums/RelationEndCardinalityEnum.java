/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum RelationEndCardinalityEnum declared at extension core.
 */
public enum RelationEndCardinalityEnum implements HybrisEnumValue
{
	/**
	 * Generated enum value for RelationEndCardinalityEnum.one declared at extension core.
	 */
	ONE("one"),
	/**
	 * Generated enum value for RelationEndCardinalityEnum.many declared at extension core.
	 */
	MANY("many");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "RelationEndCardinalityEnum";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "RelationEndCardinalityEnum";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private RelationEndCardinalityEnum(final String code)
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
