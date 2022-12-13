/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.cluster;

import de.hybris.platform.servicelayer.internal.service.AbstractService;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import org.springframework.beans.factory.annotation.Required;


public class MockClusterService extends AbstractService implements ClusterService
{
	private int clusterId;
	private long clusterIslandId;
	private Collection<String> nodeGroups;

	@Override
	public int getClusterId()
	{
		return this.clusterId;
	}

	@Required
	public void setClusterId(final int clusterId)
	{
		this.clusterId = clusterId;
	}

	@Override
	public long getClusterIslandId()
	{
		return clusterIslandId;
	}

	@Required
	public void setClusterIslandId(final long clusterIslandId)
	{
		this.clusterIslandId = clusterIslandId;
	}

	@Override
	public boolean isClusteringEnabled()
	{
		return false;
	}

	public void setClusterGroups(Collection<String> nodeGroups)
	{
		this.nodeGroups = nodeGroups == null ? null : new ArrayList<>(nodeGroups);
	}

	@Override
	public Collection<String> getClusterGroups()
	{
		return nodeGroups == null ? Collections.emptyList() : new ArrayList<>(nodeGroups);
	}

}
