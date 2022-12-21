/*
 * ----------------------------------------------------------------
 * --- WARNING: THIS FILE IS GENERATED AND WILL BE OVERWRITTEN!
 * --- Generated at 21-Dec-2022, 9:05:21 PM
 * ----------------------------------------------------------------
 *
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.core.order;

import java.io.Serializable;
import de.hybris.platform.core.enums.GroupType;
import de.hybris.platform.core.order.EntryGroup;
import java.util.List;


import java.util.Objects;
public  class EntryGroup  implements Serializable 

{

	/** Default serialVersionUID value. */
 
	private static final long serialVersionUID = 1L;

	/** <i>Generated property</i> for <code>EntryGroup.groupNumber</code> property defined at extension <code>core</code>. */
	
	private Integer groupNumber;

	/** <i>Generated property</i> for <code>EntryGroup.priority</code> property defined at extension <code>core</code>. */
	
	private Integer priority;

	/** <i>Generated property</i> for <code>EntryGroup.label</code> property defined at extension <code>core</code>. */
	
	private String label;

	/** <i>Generated property</i> for <code>EntryGroup.groupType</code> property defined at extension <code>core</code>. */
	
	private GroupType groupType;

	/** <i>Generated property</i> for <code>EntryGroup.children</code> property defined at extension <code>core</code>. */
	
	private List<EntryGroup> children;

	/** <i>Generated property</i> for <code>EntryGroup.externalReferenceId</code> property defined at extension <code>core</code>. */
	
	private String externalReferenceId;

	/** <i>Generated property</i> for <code>EntryGroup.erroneous</code> property defined at extension <code>core</code>. */
	
	private Boolean erroneous;
	
	public EntryGroup()
	{
		// default constructor
	}
	
	public void setGroupNumber(final Integer groupNumber)
	{
		this.groupNumber = groupNumber;
	}

	public Integer getGroupNumber() 
	{
		return groupNumber;
	}
	
	public void setPriority(final Integer priority)
	{
		this.priority = priority;
	}

	public Integer getPriority() 
	{
		return priority;
	}
	
	public void setLabel(final String label)
	{
		this.label = label;
	}

	public String getLabel() 
	{
		return label;
	}
	
	public void setGroupType(final GroupType groupType)
	{
		this.groupType = groupType;
	}

	public GroupType getGroupType() 
	{
		return groupType;
	}
	
	public void setChildren(final List<EntryGroup> children)
	{
		this.children = children;
	}

	public List<EntryGroup> getChildren() 
	{
		return children;
	}
	
	public void setExternalReferenceId(final String externalReferenceId)
	{
		this.externalReferenceId = externalReferenceId;
	}

	public String getExternalReferenceId() 
	{
		return externalReferenceId;
	}
	
	public void setErroneous(final Boolean erroneous)
	{
		this.erroneous = erroneous;
	}

	public Boolean getErroneous() 
	{
		return erroneous;
	}
	

}