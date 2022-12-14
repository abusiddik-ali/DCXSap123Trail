/*
 * ----------------------------------------------------------------
 * --- WARNING: THIS FILE IS GENERATED AND WILL BE OVERWRITTEN! ---
 * --- Generated at 14-Dec-2022, 10:02:07 PM                    ---
 * ----------------------------------------------------------------
 *  
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.model;

import de.hybris.bootstrap.annotations.Accessor;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.servicelayer.model.ItemModelContext;
import de.hybris.platform.task.TaskModel;

/**
 * Generated model class for type ScriptingTask first defined at extension processing.
 */
@SuppressWarnings("all")
public class ScriptingTaskModel extends TaskModel
{
	/**<i>Generated model type code constant.</i>*/
	public static final String _TYPECODE = "ScriptingTask";
	
	/** <i>Generated constant</i> - Attribute key of <code>ScriptingTask.scriptURI</code> attribute defined at extension <code>processing</code>. */
	public static final String SCRIPTURI = "scriptURI";
	
	
	/**
	 * <i>Generated constructor</i> - Default constructor for generic creation.
	 */
	public ScriptingTaskModel()
	{
		super();
	}
	
	/**
	 * <i>Generated constructor</i> - Default constructor for creation with existing context
	 * @param ctx the model context to be injected, must not be null
	 */
	public ScriptingTaskModel(final ItemModelContext ctx)
	{
		super(ctx);
	}
	
	/**
	 * <i>Generated constructor</i> - Constructor with all mandatory attributes.
	 * @deprecated since 4.1.1 Please use the default constructor without parameters
	 * @param _runnerBean initial attribute declared by type <code>ScriptingTask</code> at extension <code>processing</code>
	 * @param _scriptURI initial attribute declared by type <code>ScriptingTask</code> at extension <code>processing</code>
	 */
	@Deprecated(since = "4.1.1", forRemoval = true)
	public ScriptingTaskModel(final String _runnerBean, final String _scriptURI)
	{
		super();
		setRunnerBean(_runnerBean);
		setScriptURI(_scriptURI);
	}
	
	/**
	 * <i>Generated constructor</i> - for all mandatory and initial attributes.
	 * @deprecated since 4.1.1 Please use the default constructor without parameters
	 * @param _owner initial attribute declared by type <code>Item</code> at extension <code>core</code>
	 * @param _runnerBean initial attribute declared by type <code>ScriptingTask</code> at extension <code>processing</code>
	 * @param _scriptURI initial attribute declared by type <code>ScriptingTask</code> at extension <code>processing</code>
	 */
	@Deprecated(since = "4.1.1", forRemoval = true)
	public ScriptingTaskModel(final ItemModel _owner, final String _runnerBean, final String _scriptURI)
	{
		super();
		setOwner(_owner);
		setRunnerBean(_runnerBean);
		setScriptURI(_scriptURI);
	}
	
	
	/**
	 * <i>Generated method</i> - Getter of the <code>ScriptingTask.scriptURI</code> attribute defined at extension <code>processing</code>. 
	 * @return the scriptURI
	 */
	@Accessor(qualifier = "scriptURI", type = Accessor.Type.GETTER)
	public String getScriptURI()
	{
		return getPersistenceContext().getPropertyValue(SCRIPTURI);
	}
	
	/**
	 * <i>Generated method</i> - Setter of <code>ScriptingTask.scriptURI</code> attribute defined at extension <code>processing</code>. 
	 *  
	 * @param value the scriptURI
	 */
	@Accessor(qualifier = "scriptURI", type = Accessor.Type.SETTER)
	public void setScriptURI(final String value)
	{
		getPersistenceContext().setPropertyValue(SCRIPTURI, value);
	}
	
}
