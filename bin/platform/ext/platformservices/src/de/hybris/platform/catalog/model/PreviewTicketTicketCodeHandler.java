/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.model;


import de.hybris.platform.catalog.jalo.PreviewTicket;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import com.google.common.base.Preconditions;


public class PreviewTicketTicketCodeHandler implements DynamicAttributeHandler<String, PreviewTicketModel>
{

	@Override
	public String get(final PreviewTicketModel model)
	{
		Preconditions.checkNotNull(model.getPk(), "PreviewTicket must be saved before getting ticketCode");
		return PreviewTicket.PREVIEW_TICKET_PREFIX + model.getPk() + PreviewTicket.PREVIEW_TICKET_POSTFIX;
	}

	@Override
	public void set(final PreviewTicketModel model, final String s)
	{
		// read-only
	}
}
