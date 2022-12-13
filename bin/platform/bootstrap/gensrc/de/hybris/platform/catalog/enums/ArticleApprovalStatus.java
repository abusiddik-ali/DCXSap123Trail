/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum ArticleApprovalStatus declared at extension catalog.
 */
public enum ArticleApprovalStatus implements HybrisEnumValue
{
	/**
	 * Generated enum value for ArticleApprovalStatus.check declared at extension catalog.
	 */
	CHECK("check"),
	/**
	 * Generated enum value for ArticleApprovalStatus.approved declared at extension catalog.
	 */
	APPROVED("approved"),
	/**
	 * Generated enum value for ArticleApprovalStatus.unapproved declared at extension catalog.
	 */
	UNAPPROVED("unapproved");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "ArticleApprovalStatus";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "ArticleApprovalStatus";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private ArticleApprovalStatus(final String code)
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
