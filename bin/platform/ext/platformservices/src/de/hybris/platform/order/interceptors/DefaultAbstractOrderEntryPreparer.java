/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.order.interceptors;

import de.hybris.bootstrap.util.LocaleHelper;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.type.ComposedTypeModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.exceptions.AttributeNotSupportedException;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.model.ItemModelInternalContext;
import de.hybris.platform.servicelayer.type.TypeService;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Required;


public class DefaultAbstractOrderEntryPreparer extends AbstractAttributesModificationAwareInterceptor implements
		PrepareInterceptor
{

	protected static final int APPEND_AS_LAST = -1;
	private static final Logger LOG = Logger.getLogger(DefaultAbstractOrderEntryPreparer.class);
	private static final String NOT_AVAILABLE = "n/a";
	private TypeService typeService;
	private ConfigurationService configurationService;

	private Collection<String> attributesForOrderRecalculation;

	@Override
	public void onPrepare(final Object model, final InterceptorContext ctx) throws InterceptorException
	{
		if (model instanceof AbstractOrderEntryModel && !ctx.isRemoved(model))
		{
			final AbstractOrderEntryModel entryModel = (AbstractOrderEntryModel) model;

			//change calculated flag if any of the subjected attributed has changed, but not if calculated flag is dirty.
			if (isOneOfAttributesModified(entryModel, getAttributesForOrderRecalculation(), ctx)
					&& (ctx.isNew(model) || !ctx.isModified(entryModel, AbstractOrderEntryModel.CALCULATED)))
			{
				entryModel.setCalculated(Boolean.FALSE);
				final AbstractOrderModel ownerOrder = entryModel.getOrder();
				if (ownerOrder != null && Boolean.TRUE.equals(ownerOrder.getCalculated()))
				{
					ownerOrder.setCalculated(Boolean.FALSE);
					ctx.registerElement(ownerOrder, getModelSource(ctx, ownerOrder));
				}
			}
			if (isAttributeModified(entryModel, AbstractOrderEntryModel.PRODUCT, ctx))
			{
				//automatically generate info
				entryModel.setInfo(createEntryInformation(entryModel, ctx));
			}


			if (ctx.isNew(entryModel))
			{
				if (entryModel.getTaxValues() == null)
				{
					entryModel.setTaxValues(Collections.EMPTY_LIST);
				}
				if (entryModel.getDiscountValues() == null)
				{
					entryModel.setDiscountValues(Collections.EMPTY_LIST);
				}

			}

			if (isAttributeModified(entryModel, AbstractOrderEntryModel.ENTRYNUMBER, ctx))
			{
				final Integer entryNumber = entryModel.getEntryNumber();
				final AbstractOrderModel order = entryModel.getOrder();
				final OrderEntriesKey entriesKey = OrderEntriesKey.from(order);
				final InterceptorContext.TransientStorage storage = ctx.getTransientStorage();

				if (!storage.contains(entriesKey))
				{
					final List<AbstractOrderEntryModel> entriesFromOrder = order.getEntries();
					final ArrayList<AbstractOrderEntryModel> entriesToStore = new ArrayList<>(
							entriesFromOrder == null ? new ArrayList<>() : entriesFromOrder);
					storage.put(entriesKey, entriesToStore);
				}

				final List<AbstractOrderEntryModel> currentOrderEntries = (List<AbstractOrderEntryModel>) storage.get(entriesKey);
				if (entryNumber == null || APPEND_AS_LAST >= entryNumber.intValue())
				{
					setEntryNumberAslast(entryModel, currentOrderEntries);
				}

				final List<AbstractOrderEntryModel> newEntries = currentOrderEntries == null ? new
						ArrayList<>() : new ArrayList<>(currentOrderEntries);
				if (!newEntries.contains(entryModel))
				{
					newEntries.add(entryModel);
					storage.put(entriesKey, newEntries);
					ItemModelInternalContext itemModelContext = (ItemModelInternalContext) order.getItemModelContext();
					itemModelContext.unloadAttribute(AbstractOrderModel.ENTRIES);
				}
			}
		}
	}

	protected void setEntryNumberAslast(final AbstractOrderEntryModel entryModel, final List<AbstractOrderEntryModel>
			currentOrderEntries)
	{
		int targetEntryNumber = 0;
		if (CollectionUtils.isNotEmpty(currentOrderEntries))
		{
			//normally it's enough just to take the entryNumber from last orderEntry in the list.
			//Unfortunatelly - the context may contain some entries with entryNumber not set yet (e.g. not processed by the
			// interceptors yet). Therefore, we must find the last entry with already defined entryNumber
			for (int i = 0; i < currentOrderEntries.size(); i++)
			{
				final Integer potentialMaxEntryNumber = currentOrderEntries.get(i).getEntryNumber();
				if (!(potentialMaxEntryNumber == null || APPEND_AS_LAST >= potentialMaxEntryNumber.intValue()))
				{
					if (potentialMaxEntryNumber.intValue() >= targetEntryNumber)
					{
						targetEntryNumber = potentialMaxEntryNumber.intValue() + 1;
					}
				}
			}
		}
		entryModel.setEntryNumber(Integer.valueOf(targetEntryNumber));
	}

	protected String createEntryInformation(final AbstractOrderEntryModel newEntry, final InterceptorContext ctx)
			throws InterceptorException
	{

		final ProductModel product = newEntry.getProduct();
		// find pattern in local.properties (or project.properties)
		if (product == null)
		{
			return NOT_AVAILABLE;
		}
		else
		{
			ComposedTypeModel type = typeService.getComposedTypeForClass(product.getClass());
			String infoField = configurationService.getConfiguration().getString(
					"orderentry.infofield." + type.getCode().toLowerCase(LocaleHelper.getPersistenceLocale()), null);
			boolean hasInfoFieldAValue = true;

			while (infoField == null) //if pattern for current type is not found, search for pattern of supertype
			{
				if (type.getSuperType() == null)
				{
					hasInfoFieldAValue = false;
					break;
				}
				else
				{
					type = type.getSuperType();
					infoField = configurationService.getConfiguration().getString(
							"orderentry.infofield." + type.getCode().toLowerCase(LocaleHelper.getPersistenceLocale()), null);
				}
			}

			final String defaultstring = product.getCode(); //default string for info field, is only the product code
			final StringBuilder returnString = new StringBuilder(defaultstring);
			if (hasInfoFieldAValue) // got a pattern?
			{
				returnString.setLength(0); //throw defaultstring away
				int openTag = infoField.indexOf("${");
				int closeTag = infoField.indexOf('}');
				int start = 0;

				ItemModel item = product;

				while (openTag != -1 && closeTag != -1) // for each attribut founded in ${ }
				{
					returnString.append(infoField.substring(start, openTag));
					String code = infoField.substring((openTag + 2), closeTag);

					int attr_start = 0;
					int attr_dot = code.indexOf('.');
					while (attr_dot != -1) // is ther a 'path'?
					{
						//browse throught all instances till last dot
						try
						{
							item = goToConcreteItem(item, code.substring(attr_start, attr_dot), ctx);
						}
						catch (final AttributeNotSupportedException e)
						{
							throw new InterceptorException("pattern '" + infoField + "' contains a missing attribute '" + code);
						}
						attr_start = attr_dot + 1;
						attr_dot = code.indexOf('.', attr_start);
					}

					if (attr_start > 0 && attr_dot == -1)
					{
						code = code.substring(attr_start,
								code.length()); // all after the last dot is the attribute which should be
						// in the info field
					}

					try
					{
						final Object value = ctx.getModelService().getAttributeValue(item, code);
						returnString.append(value != null ? value.toString() : "n/a"); // could be null, so a readible 'n/a'
					}
					catch (final AttributeNotSupportedException e)
					{
						final String itemTypeCode = ctx.getModelService().getModelType(item);
						throw new InterceptorException("pattern '" + infoField + "' contains a missing attribute '" + code
								+ "' for type " + itemTypeCode, e);
					}

					start = closeTag + 1;
					openTag = infoField.indexOf("${", start);
					closeTag = infoField.indexOf('}', start);
				}
				if (start < infoField.length())
				{
					returnString.append(infoField.substring(start));
				}
			}
			return returnString.toString();
		}

	}

	protected ItemModel goToConcreteItem(final ItemModel item, final String substring, final InterceptorContext ctx)
			throws InterceptorException
	{

		final Object value = ctx.getModelService().getAttributeValue(item, substring);
		if (value instanceof ItemModel)
		{
			return (ItemModel) value;
		}
		else
		{
			throw new InterceptorException("Unknown type " + substring);
		}

	}

	@Required
	public void setTypeService(final TypeService typeService)
	{
		this.typeService = typeService;
	}


	@Required
	public void setConfigurationService(final ConfigurationService configurationService)
	{
		this.configurationService = configurationService;
	}

	protected Collection<String> getAttributesForOrderRecalculation()
	{
		if (attributesForOrderRecalculation != null)
		{
			return attributesForOrderRecalculation;
		}
		else
		{
			//fallback list of attributes (the same set results from jalo layer logic)
			return Arrays.asList(AbstractOrderEntryModel.PRODUCT, AbstractOrderEntryModel.QUANTITY, AbstractOrderEntryModel.UNIT,
					AbstractOrderEntryModel.BASEPRICE, AbstractOrderEntryModel.TAXVALUES,
					AbstractOrderEntryModel.DISCOUNTVALUES,
					AbstractOrderEntryModel.GIVEAWAY, AbstractOrderEntryModel.REJECTED);
		}

	}

	public void setAttributesForOrderRecalculation(final Collection<String> attributesForOrderRecalculation)
	{
		this.attributesForOrderRecalculation = attributesForOrderRecalculation;
	}

	@Override
	public Logger getLogger()
	{
		return LOG;
	}

	protected Object getModelSource(final InterceptorContext ctx, final ItemModel model)
	{
		if (ctx.isNew(model))
		{
			return null;
		}
		else
		{
			return ctx.getModelService().getSource(model);
		}
	}

	private static class OrderEntriesKey
	{
		private final AbstractOrderModel order;

		private OrderEntriesKey(final AbstractOrderModel order)
		{
			this.order = order;
		}

		private static OrderEntriesKey from(AbstractOrderModel order)
		{
			return new OrderEntriesKey(order);
		}

		@Override
		public boolean equals(final Object obj)
		{
			if (obj == null || obj == this)
			{
				return this == obj;
			}

			return obj instanceof OrderEntriesKey && order == (((OrderEntriesKey) obj).order);
		}

		@Override
		public int hashCode()
		{
			return System.identityHashCode(order);
		}
	}

}
