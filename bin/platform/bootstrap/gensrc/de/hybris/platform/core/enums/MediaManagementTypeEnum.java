/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum MediaManagementTypeEnum declared at extension core.
 */
public enum MediaManagementTypeEnum implements HybrisEnumValue
{
	/**
	 * Generated enum value for MediaManagementTypeEnum.FILES declared at extension core.
	 */
	FILES("FILES"),
	/**
	 * Generated enum value for MediaManagementTypeEnum.SSH declared at extension core.
	 */
	SSH("SSH"),
	/**
	 * Generated enum value for MediaManagementTypeEnum.FTP declared at extension core.
	 */
	FTP("FTP");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "MediaManagementTypeEnum";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "MediaManagementTypeEnum";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private MediaManagementTypeEnum(final String code)
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
