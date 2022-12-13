/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.audit.internal.conditional;

import static java.util.stream.Collectors.toList;
import static org.mockito.BDDMockito.given;

import de.hybris.platform.core.PK;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.directpersistence.cache.SLDDataContainer;
import de.hybris.platform.directpersistence.cache.SLDDataContainerProvider;
import de.hybris.platform.persistence.audit.AuditChangeFilter;
import de.hybris.platform.persistence.audit.gateway.AuditRecord;
import de.hybris.platform.persistence.audit.gateway.AuditSearchQuery;
import de.hybris.platform.persistence.audit.gateway.ReadAuditGateway;
import de.hybris.platform.persistence.property.TypeInfoMap;
import de.hybris.platform.servicelayer.type.TypeService;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.assertj.core.api.AbstractAssert;
import org.mockito.Mockito;

import com.google.common.collect.Lists;

public class ConditionalAuditTestUtils
{
	private static final PK typePk = PK.createFixedUUIDPK(2, 2);

	public static List<AuditRecord> getAuditRecordsFor(final String type, final PK pk)
	{
		final ReadAuditGateway readAuditGateway = Registry.getApplicationContext()
		                                                  .getBean("readAuditGateway", ReadAuditGateway.class);
		return readAuditGateway.search(AuditSearchQuery.forType(type).withPkSearchRules(pk).build()).collect(toList());
	}

	public static AuditChangeFilter auditTitlesNotEndingWithAudit1andAudit2(
			final SLDDataContainerProvider sldDataContainerProvider, final TypeService typeService)
	{
		return new ConditionalAuditChangeFilter(
				"audit.test/conditional-audit-address-title.xml", sldDataContainerProvider, typeService,
				Registry.getCurrentTenant().getConfig());
	}

	public static SLDDataContainer createDataContainer(final String typeCode, final PK pk, final Map<String, Object> attributes)
	{
		final TypeInfoMap typeInfoMap = Mockito.mock(TypeInfoMap.class);
		given(typeInfoMap.getCode()).willReturn(typeCode);

		final List<SLDDataContainer.AttributeValue> containerAttributes = attributes.entrySet().stream() //
		                                                                            .map(i -> newAttribute(i.getKey(),
				                                                                            i.getValue())) //
		                                                                            .collect(Collectors.toList());

		final SLDDataContainer container = SLDDataContainer.builder() //
		                                                   .withPk(pk) //
		                                                   .withTypePk(typePk) //
		                                                   .withTypeInfoMap(typeInfoMap) //
		                                                   .withAttributes(Lists.newArrayList(containerAttributes)) //
		                                                   .withVersion(Long.valueOf(1)).build();

		return container;
	}

	private static SLDDataContainer.AttributeValue newAttribute(final String attr, final Object value)
	{
		return new SLDDataContainer.AttributeValue(attr, value);
	}

	public static class ItemModelAuditRecordsAssert
			extends AbstractAssert<ItemModelAuditRecordsAssert, ItemModel>
	{

		public ItemModelAuditRecordsAssert(final ItemModel actual)
		{
			super(actual, ItemModelAuditRecordsAssert.class);
		}

		public static ItemModelAuditRecordsAssert assertThat(final ItemModel actual)
		{
			return new ItemModelAuditRecordsAssert(actual);
		}

		public ItemModelAuditRecordsAssert hasRecordedAudits(final int number)
		{
			final ReadAuditGateway readAuditGateway = Registry.getApplicationContext()
			                                                  .getBean("readAuditGateway", ReadAuditGateway.class);


			final List<AuditRecord> recordedAudits = readAuditGateway.search(
					AuditSearchQuery.forType(actual.getItemtype())
					                .withPkSearchRules(actual.getPk())
					                .build()).collect(toList());

			final int recordedNumber = recordedAudits.size();

			if (number != recordedNumber)
			{
				failWithMessage("Expected a number of recorded audit records to be <%d> but was <%d>", number, recordedNumber);
			}

			return this;
		}
	}
}
