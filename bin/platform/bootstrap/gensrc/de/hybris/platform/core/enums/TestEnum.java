/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum TestEnum declared at extension core.
 */
public enum TestEnum implements HybrisEnumValue
{
	/**
	 * Generated enum value for TestEnum.testValue1 declared at extension core.
	 */
	TESTVALUE1("testValue1"),
	/**
	 * Generated enum value for TestEnum.testValue2 declared at extension core.
	 */
	TESTVALUE2("testValue2"),
	/**
	 * Generated enum value for TestEnum.testValue3 declared at extension core.
	 */
	TESTVALUE3("testValue3"),
	/**
	 * Generated enum value for TestEnum.testValue4 declared at extension core.
	 */
	TESTVALUE4("testValue4");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "TestEnum";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "TestEnum";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private TestEnum(final String code)
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
