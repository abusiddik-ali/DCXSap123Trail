/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.strategies.ordercloning.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.order.AbstractOrderEntryTypeService;
import de.hybris.platform.order.strategies.ordercloning.CloneAbstractOrderHook;
import de.hybris.platform.order.strategies.ordercloning.CloneAbstractOrderStrategy;
import de.hybris.platform.servicelayer.internal.model.impl.ItemModelCloneCreator;
import de.hybris.platform.servicelayer.internal.model.impl.ItemModelCloneCreator.CopyContext;
import de.hybris.platform.servicelayer.type.TypeService;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Use {@link ItemModelCloneCreator} to clone one of subtype of
 * {@link AbstractOrderModel} to other (or the same) subtype.
 */
public class DefaultCloneAbstractOrderStrategy implements CloneAbstractOrderStrategy
{

	private final TypeService typeService;
	private final ItemModelCloneCreator itemModelCloneCreator;
	private final AbstractOrderEntryTypeService abstractOrderEntryTypeService;
	private final Set<String> skippedAttributes;
	private List<CloneAbstractOrderHook> cloneHooks = Collections.emptyList();

	public DefaultCloneAbstractOrderStrategy(final TypeService typeService,
	                                         final ItemModelCloneCreator itemModelCloneCreator,
			final AbstractOrderEntryTypeService abstractOrderEntryTypeService, final List<String> skippedAttributes)
	{
		super();
		this.typeService = typeService;
		this.itemModelCloneCreator = itemModelCloneCreator;
		this.abstractOrderEntryTypeService = abstractOrderEntryTypeService;
		this.skippedAttributes = Set.copyOf(skippedAttributes);
	}

	public DefaultCloneAbstractOrderStrategy(final TypeService typeService, final ItemModelCloneCreator itemModelCloneCreator,
			final AbstractOrderEntryTypeService abstractOrderEntryTypeService)
	{
		this(typeService, itemModelCloneCreator, abstractOrderEntryTypeService, new ArrayList<String>());
	}

	/**
	 * Create clone of original and change the type of order and orderEntries
	 *
	 * @param _orderType               clone order type
	 * @param _entryType               clone order entry type
	 * @param original                 original order
	 * @param code                     clone code
	 * @param abstractOrderClassResult super class of clone (usually OrderModel or
	 *                                 CartModel)
	 */
	@Override
	public <T extends AbstractOrderModel> T clone(final ComposedTypeModel _orderType,
	                                              final ComposedTypeModel _entryType, final AbstractOrderModel original,
	                                              final String code,
	                                              final Class abstractOrderClassResult, final Class abstractOrderEntryClassResult)
	{
		validateParameterNotNull(original, "original must not be null!");
		validateParameterNotNull(abstractOrderClassResult, "abstractOrderClassResult must not be null!");
		validateParameterNotNull(abstractOrderEntryClassResult, "abstractOrderEntryClassResult must not be null!");

		final ComposedTypeModel orderType = getOrderType(_orderType, original, abstractOrderClassResult);
		final ComposedTypeModel entryType = getOrderEntryType(_entryType, original, abstractOrderClassResult,
				abstractOrderEntryClassResult);

		beforeClone(original, abstractOrderClassResult);

		final OrderCopyContext copyContext = createCloneCopyContext(entryType);
		adjustCopyContext(original, copyContext);

		final T orderClone = (T) itemModelCloneCreator.copy(orderType, original, copyContext);
		if (code != null)
		{
			orderClone.setCode(code);
		}
		postProcess(original, orderClone);

		afterClone(original, orderClone, abstractOrderClassResult);
		return orderClone;
	}

	protected OrderCopyContext createCloneCopyContext(final ComposedTypeModel entryType)
	{
		return new OrderCopyContext(entryType, skippedAttributes);
	}

	/**
	 * Clone the entries of the <b>original</b> and change the type according to the
	 * <b>entryType</b> given.
	 *
	 * @param _entryType desited {@link ComposedTypeModel} of the cloned cart
	 *                   entries .
	 * @param original   original {@link AbstractOrderModel}
	 * @return collection of cloned entries
	 */
	@Override
	public <T extends AbstractOrderEntryModel> List<T> cloneEntries(final ComposedTypeModel _entryType,
	                                                                final AbstractOrderModel original)
	{
		validateParameterNotNull(original, "original must not be null!");
		ComposedTypeModel entryType = null;
		if (_entryType != null)
		{
			entryType = _entryType;
		}
		else
		{
			entryType = abstractOrderEntryTypeService.getAbstractOrderEntryType(original);
		}

		beforeCloneEntries(original);

		final OrderCopyContext copyContext = createCloneEntriesCopyContext();
		final List<ItemModel> entriesItems = new ArrayList<ItemModel>(original.getEntries());

		final List<ItemModel> itemClones = itemModelCloneCreator.copyAll(entryType, entriesItems, copyContext);
		if (itemClones != null)
		{
			final List<T> entriesClones = new ArrayList<T>(itemClones.size());
			for (final ItemModel itemClone : itemClones)
			{
				entriesClones.add((T) itemClone);
			}
			postProcessEntries(original.getEntries(), entriesClones);
			afterCloneEntries(original, entriesClones);
			return entriesClones;
		}
		else
		{
			afterCloneEntries(original, Collections.<T> emptyList());
			return Collections.<T> emptyList();
		}

	}

	final Map<AbstractOrderEntryModel, Integer> getOriginalEntryNumberMappings(
			final AbstractOrderModel original)
	{
		final Map<AbstractOrderEntryModel, Integer> originalEntryNumberMappings = new HashMap<>();
		for (final AbstractOrderEntryModel order : original.getEntries())
		{
			originalEntryNumberMappings.put(order,order.getEntryNumber());
		}
		return originalEntryNumberMappings;
	}


	protected OrderCopyContext createCloneEntriesCopyContext()
	{
		final Set<String> adjustedSkippedAttributed = new HashSet<>(skippedAttributes);
		adjustedSkippedAttributed.add(AbstractOrderEntryModel.ORDER);
		return new OrderCopyContext(null, adjustedSkippedAttributed);
	}

	protected <T extends AbstractOrderModel> void afterClone(final AbstractOrderModel original, final T clone,
			final Class abstractOrderClassResult)
	{
		cloneHooks.forEach(hook -> hook.afterClone(original, clone, abstractOrderClassResult));
	}

	protected void beforeClone(final AbstractOrderModel original, final Class abstractOrderClassResult)
	{
		cloneHooks.forEach(hook -> hook.beforeClone(original, abstractOrderClassResult));
	}

	protected void beforeCloneEntries(final AbstractOrderModel original)
	{
		cloneHooks.forEach(hook -> hook.beforeCloneEntries(original));
	}

	protected <T extends AbstractOrderEntryModel> void afterCloneEntries(final AbstractOrderModel original,
			final List<T> clonedEntries)
	{
		cloneHooks.forEach(hook -> hook.afterCloneEntries(original, clonedEntries));
	}

	protected Map<AbstractOrderEntryModel, Integer> adjustEntryNumbers(final Map<AbstractOrderEntryModel, Integer> entryNumberMappings)
	{
		cloneHooks.forEach(hook -> hook.adjustEntryNumbers(entryNumberMappings));
		return entryNumberMappings;
	}

	private void adjustCopyContext(final AbstractOrderModel original, final OrderCopyContext copyContext)
	{
		final Map<AbstractOrderEntryModel, Integer> adjustedEntryNumbers = adjustEntryNumbers(
				getOriginalEntryNumberMappings(original));
		establishEntryNumberPresetForCopyContext(adjustedEntryNumbers, copyContext);
	}


	private void establishEntryNumberPresetForCopyContext(final Map<AbstractOrderEntryModel, Integer> adjustedEntryNumbers,
	                                                      final OrderCopyContext copyContext)
	{
		adjustedEntryNumbers.forEach((item, value) -> copyContext.addPreset(item, AbstractOrderEntryModel.ENTRYNUMBER, value));
	}


	private <T extends AbstractOrderModel> ComposedTypeModel getOrderType(final ComposedTypeModel orderType,
	                                                                      final AbstractOrderModel original, final Class<T> clazz)
	{
		if (orderType != null)
		{
			return orderType;
		}

		if (clazz.isAssignableFrom(original.getClass()))
		{
			return typeService.getComposedTypeForClass(original.getClass());

		}

		return typeService.getComposedTypeForClass(clazz);
	}

	private <E extends AbstractOrderEntryModel, T extends AbstractOrderModel> ComposedTypeModel getOrderEntryType(
			final ComposedTypeModel entryType, final AbstractOrderModel original, final Class<T> orderClazz,
			final Class<E> clazz)
	{
		if (entryType != null)
		{
			return entryType;
		}

		if (orderClazz.isAssignableFrom(original.getClass()))
		{
			return abstractOrderEntryTypeService.getAbstractOrderEntryType(original);
		}

		return typeService.getComposedTypeForClass(clazz);
	}

	private static <T extends AbstractOrderEntryModel> void postProcessEntries(
			final List<AbstractOrderEntryModel> originalEntries, final List<T> entriesClones)
	{
		for (final AbstractOrderEntryModel entry : entriesClones)
		{
			entry.setCalculated(Boolean.FALSE);
		}
	}

	protected void postProcess(final AbstractOrderModel original, final AbstractOrderModel copy)
	{
		copyTotalTaxValues(original, copy);
		copyCalculatedFlag(original, copy);
	}

	protected void copyTotalTaxValues(final AbstractOrderModel original, final AbstractOrderModel copy)
	{
		copy.setTotalTaxValues(original.getTotalTaxValues());

	}

	protected void copyCalculatedFlag(final AbstractOrderModel original, final AbstractOrderModel copy)
	{
		copy.setCalculated(original.getCalculated());

		final List<AbstractOrderEntryModel> originalEntries = original.getEntries();
		final List<AbstractOrderEntryModel> copyEntries = copy.getEntries();

		final int copyEntriesSize = copyEntries == null ? 0 : copyEntries.size();

		if (originalEntries.size() != copyEntriesSize)
		{
			throw new IllegalStateException("different entry numbers in original and copied order ( "
					+ originalEntries.size() + "<>" + copyEntriesSize + ")");
		}

		if(copyEntries != null)
		{
			for (int i = 0; i < originalEntries.size(); i++)
			{
				final AbstractOrderEntryModel originalEntry = originalEntries.get(i);
				final AbstractOrderEntryModel copyEntry = copyEntries.get(i);
				copyEntry.setCalculated(originalEntry.getCalculated());
			}
		}
	}

	/**
	 * CopyContext extension to account for order entries and skipped attributes in
	 * this document context
	 */
	protected static class OrderCopyContext extends CopyContext
	{
		private final ComposedTypeModel entryType;
		private final Set<String> skippedAttributes;

		protected OrderCopyContext(final ComposedTypeModel entryType, final Set<String> skippedAttributes)
		{
			this.entryType = entryType;
			this.skippedAttributes = skippedAttributes;
		}

		@Override
		public ComposedTypeModel getTargetType(final ItemModel originalModel)
		{
			if (entryType != null && originalModel instanceof AbstractOrderEntryModel)
			{
				return entryType;
			}
			return super.getTargetType(originalModel);
		}

		@Override
		protected boolean skipAttribute(final Object original, final String qualifier)
		{
			if (skippedAttributes.contains(qualifier))
			{
				return true;
			}
			else
			{
				return super.skipAttribute(original, qualifier);
			}
		}
	}

	public void setCloneHooks(final List<CloneAbstractOrderHook> cloneHooks)
	{
		this.cloneHooks = cloneHooks;
	}

}
