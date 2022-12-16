/*
 * ----------------------------------------------------------------
 * --- WARNING: THIS FILE IS GENERATED AND WILL BE OVERWRITTEN! ---
 * --- Generated at 16-Dec-2022, 6:42:52 PM                     ---
 * ----------------------------------------------------------------
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.commons.model;

import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.commons.model.MediaFormatterModel;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.servicelayer.model.ItemModelContext;

/**
 * Generated model class for type XMLTransformFormatter first defined at extension commons.
 */
@SuppressWarnings("all")
public class XMLTransformFormatterModel extends MediaFormatterModel
{
	/**<i>Generated model type code constant.</i>*/
	public static final String _TYPECODE = "XMLTransformFormatter";
	
	
	/**
	 * <i>Generated constructor</i> - Default constructor for generic creation.
	 */
	public XMLTransformFormatterModel()
	{
		super();
	}
	
	/**
	 * <i>Generated constructor</i> - Default constructor for creation with existing context
	 * @param ctx the model context to be injected, must not be null
	 */
	public XMLTransformFormatterModel(final ItemModelContext ctx)
	{
		super(ctx);
	}
	
	/**
	 * <i>Generated constructor</i> - Constructor with all mandatory attributes.
	 * @deprecated since 4.1.1 Please use the default constructor without parameters
	 * @param _catalogVersion initial attribute declared by type <code>Formatter</code> at extension <code>catalog</code>
	 * @param _code initial attribute declared by type <code>Media</code> at extension <code>core</code>
	 * @param _inputMimeType initial attribute declared by type <code>MediaFormatter</code> at extension <code>commons</code>
	 * @param _outputMimeType initial attribute declared by type <code>Formatter</code> at extension <code>commons</code>
	 */
	@Deprecated(since = "4.1.1", forRemoval = true)
	public XMLTransformFormatterModel(final CatalogVersionModel _catalogVersion, final String _code, final String _inputMimeType, final String _outputMimeType)
	{
		super();
		setCatalogVersion(_catalogVersion);
		setCode(_code);
		setInputMimeType(_inputMimeType);
		setOutputMimeType(_outputMimeType);
	}
	
	/**
	 * <i>Generated constructor</i> - for all mandatory and initial attributes.
	 * @deprecated since 4.1.1 Please use the default constructor without parameters
	 * @param _catalogVersion initial attribute declared by type <code>Formatter</code> at extension <code>catalog</code>
	 * @param _code initial attribute declared by type <code>Media</code> at extension <code>core</code>
	 * @param _inputMimeType initial attribute declared by type <code>MediaFormatter</code> at extension <code>commons</code>
	 * @param _outputMimeType initial attribute declared by type <code>Formatter</code> at extension <code>commons</code>
	 * @param _owner initial attribute declared by type <code>Item</code> at extension <code>core</code>
	 */
	@Deprecated(since = "4.1.1", forRemoval = true)
	public XMLTransformFormatterModel(final CatalogVersionModel _catalogVersion, final String _code, final String _inputMimeType, final String _outputMimeType, final ItemModel _owner)
	{
		super();
		setCatalogVersion(_catalogVersion);
		setCode(_code);
		setInputMimeType(_inputMimeType);
		setOutputMimeType(_outputMimeType);
		setOwner(_owner);
	}
	
	
}
