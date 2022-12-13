/*
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.workflow.enums;

import de.hybris.platform.core.HybrisEnumValue;

/**
 * Generated enum WorkflowActionType declared at extension workflow.
 */
public enum WorkflowActionType implements HybrisEnumValue
{
	/**
	 * Generated enum value for WorkflowActionType.start declared at extension workflow.
	 */
	START("start"),
	/**
	 * Generated enum value for WorkflowActionType.end declared at extension workflow.
	 */
	END("end"),
	/**
	 * Generated enum value for WorkflowActionType.normal declared at extension workflow.
	 */
	NORMAL("normal");
	 
	/**<i>Generated model type code constant.</i>*/
	public final static String _TYPECODE = "WorkflowActionType";
	
	/**<i>Generated simple class name constant.</i>*/
	public final static String SIMPLE_CLASSNAME = "WorkflowActionType";
	
	/** The code of this enum.*/
	private final String code;
	
	/**
	 * Creates a new enum value for this enum type.
	 *  
	 * @param code the enum value code
	 */
	private WorkflowActionType(final String code)
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
