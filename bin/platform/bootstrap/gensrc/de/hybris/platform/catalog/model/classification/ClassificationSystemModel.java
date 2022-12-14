/*
 * ----------------------------------------------------------------
 * --- WARNING: THIS FILE IS GENERATED AND WILL BE OVERWRITTEN! ---
 * --- Generated at 14-Dec-2022, 8:42:43 PM                     ---
 * ----------------------------------------------------------------
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.model.classification;

import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.servicelayer.model.ItemModelContext;

/**
 * Generated model class for type ClassificationSystem first defined at extension catalog.
 */
@SuppressWarnings("all")
public class ClassificationSystemModel extends CatalogModel
{
	/**<i>Generated model type code constant.</i>*/
	public static final String _TYPECODE = "ClassificationSystem";
	
	
	/**
	 * <i>Generated constructor</i> - Default constructor for generic creation.
	 */
	public ClassificationSystemModel()
	{
		super();
	}
	
	/**
	 * <i>Generated constructor</i> - Default constructor for creation with existing context
	 * @param ctx the model context to be injected, must not be null
	 */
	public ClassificationSystemModel(final ItemModelContext ctx)
	{
		super(ctx);
	}
	
	/**
	 * <i>Generated constructor</i> - Constructor with all mandatory attributes.
	 * @deprecated since 4.1.1 Please use the default constructor without parameters
	 * @param _id initial attribute declared by type <code>Catalog</code> at extension <code>catalog</code>
	 */
	@Deprecated(since = "4.1.1", forRemoval = true)
	public ClassificationSystemModel(final String _id)
	{
		super();
		setId(_id);
	}
	
	/**
	 * <i>Generated constructor</i> - for all mandatory and initial attributes.
	 * @deprecated since 4.1.1 Please use the default constructor without parameters
	 * @param _id initial attribute declared by type <code>Catalog</code> at extension <code>catalog</code>
	 * @param _owner initial attribute declared by type <code>Item</code> at extension <code>core</code>
	 */
	@Deprecated(since = "4.1.1", forRemoval = true)
	public ClassificationSystemModel(final String _id, final ItemModel _owner)
	{
		super();
		setId(_id);
		setOwner(_owner);
	}
	
	
}
