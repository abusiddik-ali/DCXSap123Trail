/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum ProductDifferenceMode declared at extension catalog.
 */
public enum ProductDifferenceMode implements HybrisEnumValue
{
	/**
	 * Generated enum value for ProductDifferenceMode.product_new declared at extension catalog.
	 */
	PRODUCT_NEW("product_new"),
	/**
	 * Generated enum value for ProductDifferenceMode.product_removed declared at extension catalog.
	 */
	PRODUCT_REMOVED("product_removed"),
	/**
	 * Generated enum value for ProductDifferenceMode.product_pricedifference declared at extension catalog.
	 */
	PRODUCT_PRICEDIFFERENCE("product_pricedifference");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "ProductDifferenceMode";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "ProductDifferenceMode";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private ProductDifferenceMode(final String code)
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
