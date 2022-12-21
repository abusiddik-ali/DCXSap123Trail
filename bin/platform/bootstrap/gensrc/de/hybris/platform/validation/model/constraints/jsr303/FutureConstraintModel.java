/*
 * ----------------------------------------------------------------
 * --- WARNING: THIS FILE IS GENERATED AND WILL BE OVERWRITTEN! ---
 * --- Generated at 21-Dec-2022, 4:02:22 PM                     ---
 * ----------------------------------------------------------------
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.validation.model.constraints.jsr303;

import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.servicelayer.model.ItemModelContext;
import de.hybris.platform.validation.model.constraints.AttributeConstraintModel;

/**
 * Generated model class for type FutureConstraint first defined at extension validation.
 * <p>
 * Future JSR 303 compatible constraint class.
 */
@SuppressWarnings("all")
public class FutureConstraintModel extends AttributeConstraintModel
{
	/**<i>Generated model type code constant.</i>*/
	public static final String _TYPECODE = "FutureConstraint";
	
	
	/**
	 * <i>Generated constructor</i> - Default constructor for generic creation.
	 */
	public FutureConstraintModel()
	{
		super();
	}
	
	/**
	 * <i>Generated constructor</i> - Default constructor for creation with existing context
	 * @param ctx the model context to be injected, must not be null
	 */
	public FutureConstraintModel(final ItemModelContext ctx)
	{
		super(ctx);
	}
	
	/**
	 * <i>Generated constructor</i> - Constructor with all mandatory attributes.
	 * @deprecated since 4.1.1 Please use the default constructor without parameters
	 * @param _annotation initial attribute declared by type <code>FutureConstraint</code> at extension <code>validation</code>
	 * @param _id initial attribute declared by type <code>AbstractConstraint</code> at extension <code>validation</code>
	 */
	@Deprecated(since = "4.1.1", forRemoval = true)
	public FutureConstraintModel(final Class _annotation, final String _id)
	{
		super();
		setAnnotation(_annotation);
		setId(_id);
	}
	
	/**
	 * <i>Generated constructor</i> - for all mandatory and initial attributes.
	 * @deprecated since 4.1.1 Please use the default constructor without parameters
	 * @param _annotation initial attribute declared by type <code>FutureConstraint</code> at extension <code>validation</code>
	 * @param _id initial attribute declared by type <code>AbstractConstraint</code> at extension <code>validation</code>
	 * @param _owner initial attribute declared by type <code>Item</code> at extension <code>core</code>
	 */
	@Deprecated(since = "4.1.1", forRemoval = true)
	public FutureConstraintModel(final Class _annotation, final String _id, final ItemModel _owner)
	{
		super();
		setAnnotation(_annotation);
		setId(_id);
		setOwner(_owner);
	}
	
	
}
