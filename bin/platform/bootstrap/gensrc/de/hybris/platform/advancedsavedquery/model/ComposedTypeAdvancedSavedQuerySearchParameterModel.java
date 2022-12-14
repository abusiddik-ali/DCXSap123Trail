/*
 * ----------------------------------------------------------------
 * --- WARNING: THIS FILE IS GENERATED AND WILL BE OVERWRITTEN! ---
 * --- Generated at 14-Dec-2022, 10:02:07 PM                    ---
 * ----------------------------------------------------------------
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.advancedsavedquery.model;

import de.hybris.platform.advancedsavedquery.model.TypedAdvancedSavedQuerySearchParameterModel;
import de.hybris.platform.advancedsavedquery.model.WherePartModel;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.servicelayer.model.ItemModelContext;

/**
 * Generated model class for type ComposedTypeAdvancedSavedQuerySearchParameter first defined at extension advancedsavedquery.
 */
@SuppressWarnings("all")
public class ComposedTypeAdvancedSavedQuerySearchParameterModel extends TypedAdvancedSavedQuerySearchParameterModel
{
	/**<i>Generated model type code constant.</i>*/
	public static final String _TYPECODE = "ComposedTypeAdvancedSavedQuerySearchParameter";
	
	
	/**
	 * <i>Generated constructor</i> - Default constructor for generic creation.
	 */
	public ComposedTypeAdvancedSavedQuerySearchParameterModel()
	{
		super();
	}
	
	/**
	 * <i>Generated constructor</i> - Default constructor for creation with existing context
	 * @param ctx the model context to be injected, must not be null
	 */
	public ComposedTypeAdvancedSavedQuerySearchParameterModel(final ItemModelContext ctx)
	{
		super(ctx);
	}
	
	/**
	 * <i>Generated constructor</i> - Constructor with all mandatory attributes.
	 * @deprecated since 4.1.1 Please use the default constructor without parameters
	 * @param _enclosingType initial attribute declared by type <code>ComposedTypeAdvancedSavedQuerySearchParameter</code> at extension <code>advancedsavedquery</code>
	 * @param _searchParameterName initial attribute declared by type <code>TypedAdvancedSavedQuerySearchParameter</code> at extension <code>advancedsavedquery</code>
	 * @param _wherePart initial attribute declared by type <code>AbstractAdvancedSavedQuerySearchParameter</code> at extension <code>advancedsavedquery</code>
	 */
	@Deprecated(since = "4.1.1", forRemoval = true)
	public ComposedTypeAdvancedSavedQuerySearchParameterModel(final ComposedTypeModel _enclosingType, final String _searchParameterName, final WherePartModel _wherePart)
	{
		super();
		setEnclosingType(_enclosingType);
		setSearchParameterName(_searchParameterName);
		setWherePart(_wherePart);
	}
	
	/**
	 * <i>Generated constructor</i> - for all mandatory and initial attributes.
	 * @deprecated since 4.1.1 Please use the default constructor without parameters
	 * @param _enclosingType initial attribute declared by type <code>ComposedTypeAdvancedSavedQuerySearchParameter</code> at extension <code>advancedsavedquery</code>
	 * @param _owner initial attribute declared by type <code>Item</code> at extension <code>core</code>
	 * @param _searchParameterName initial attribute declared by type <code>TypedAdvancedSavedQuerySearchParameter</code> at extension <code>advancedsavedquery</code>
	 * @param _wherePart initial attribute declared by type <code>AbstractAdvancedSavedQuerySearchParameter</code> at extension <code>advancedsavedquery</code>
	 */
	@Deprecated(since = "4.1.1", forRemoval = true)
	public ComposedTypeAdvancedSavedQuerySearchParameterModel(final ComposedTypeModel _enclosingType, final ItemModel _owner, final String _searchParameterName, final WherePartModel _wherePart)
	{
		super();
		setEnclosingType(_enclosingType);
		setOwner(_owner);
		setSearchParameterName(_searchParameterName);
		setWherePart(_wherePart);
	}
	
	
}
