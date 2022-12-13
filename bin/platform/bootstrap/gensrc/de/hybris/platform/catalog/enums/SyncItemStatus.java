/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum SyncItemStatus declared at extension catalog.
 */
public enum SyncItemStatus implements HybrisEnumValue
{
	/**
	 * Generated enum value for SyncItemStatus.IN_SYNC declared at extension catalog.
	 */
	IN_SYNC("IN_SYNC"),
	/**
	 * Generated enum value for SyncItemStatus.NOT_SYNC declared at extension catalog.
	 */
	NOT_SYNC("NOT_SYNC"),
	/**
	 * Generated enum value for SyncItemStatus.COUNTERPART_MISSING declared at extension catalog.
	 */
	COUNTERPART_MISSING("COUNTERPART_MISSING"),
	/**
	 * Generated enum value for SyncItemStatus.ITEM_MISSING declared at extension catalog.
	 */
	ITEM_MISSING("ITEM_MISSING"),
	/**
	 * Generated enum value for SyncItemStatus.NOT_APPLICABLE declared at extension catalog.
	 */
	NOT_APPLICABLE("NOT_APPLICABLE"),
	/**
	 * Generated enum value for SyncItemStatus.IN_PROGRESS declared at extension catalog.
	 */
	IN_PROGRESS("IN_PROGRESS"),
	/**
	 * Generated enum value for SyncItemStatus.SYNC_RULES_VIOLATED declared at extension catalog.
	 */
	SYNC_RULES_VIOLATED("SYNC_RULES_VIOLATED");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "SyncItemStatus";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "SyncItemStatus";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private SyncItemStatus(final String code)
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
