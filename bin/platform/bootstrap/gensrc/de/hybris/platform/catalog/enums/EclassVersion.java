/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum EclassVersion declared at extension catalog.
 */
public enum EclassVersion implements HybrisEnumValue
{
	/**
	 * Generated enum value for EclassVersion.VERSION_4_10 declared at extension catalog.
	 */
	VERSION_4_10("VERSION_4_10"),
	/**
	 * Generated enum value for EclassVersion.VERSION_5_00 declared at extension catalog.
	 */
	VERSION_5_00("VERSION_5_00"),
	/**
	 * Generated enum value for EclassVersion.VERSION_5_10 declared at extension catalog.
	 */
	VERSION_5_10("VERSION_5_10");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "EclassVersion";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "EclassVersion";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private EclassVersion(final String code)
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
