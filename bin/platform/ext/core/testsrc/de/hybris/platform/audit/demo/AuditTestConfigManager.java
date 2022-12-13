/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.audit.demo;

import de.hybris.platform.persistence.audit.internal.AuditEnablementService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;

import java.util.HashMap;
import java.util.Map;


public class AuditTestConfigManager
{

	private final Map<String, PropertyConfigSwitcher> auditedTypes = new HashMap<>();
	private final AuditEnablementService auditEnablementService;

	/**
	 * @deprecated since 6.6 in favor of {@link AuditTestConfigManager#AuditTestConfigManager(AuditEnablementService)}.
	 */
	@Deprecated(since = "6.6", forRemoval = true)
	public AuditTestConfigManager(
			final de.hybris.platform.directpersistence.audit.internal.AuditEnablementService auditEnablementService)
	{
		this(auditEnablementService.getDelegate());
	}

	public AuditTestConfigManager(final AuditEnablementService auditEnablementService)
	{
		this.auditEnablementService = auditEnablementService;
	}

	public void enableAuditingForTypes(final String... types)
	{
		for (final String type : types)
		{
			final PropertyConfigSwitcher switcher = getPropertyConfigSwitcher(type.toLowerCase());
			switcher.switchToValue("true");
		}

		auditEnablementService.refreshConfiguredAuditTypes();
	}

	public void disableAuditingForTypes(final String... types)
	{
		for (final String type : types)
		{
			final PropertyConfigSwitcher switcher = getPropertyConfigSwitcher(type.toLowerCase());
			switcher.switchToValue("false");
		}

		auditEnablementService.refreshConfiguredAuditTypes();
	}

	private PropertyConfigSwitcher getPropertyConfigSwitcher(final String type)
	{
		final PropertyConfigSwitcher switcher;
		if (auditedTypes.containsKey(type))
		{
			switcher = auditedTypes.get(type);
		}
		else
		{
			switcher = new PropertyConfigSwitcher("audit." + type + ".enabled");
			auditedTypes.put(type, switcher);
		}
		return switcher;
	}

	public void resetAuditConfiguration()
	{
		for (final Map.Entry<String, PropertyConfigSwitcher> entry : auditedTypes.entrySet())
		{
			entry.getValue().switchBackToDefault();
		}

		auditEnablementService.refreshConfiguredAuditTypes();
	}
}
