/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.job.sort;

import java.util.Collection;
import java.util.List;


/**
 * Orders an unsorted collection to ordered one.
 *
 * @spring.bean typesSorter
 * @since 4.3
 */
public interface Sorter<T>
{
	List<T> sort(Collection<T> unordered);
}
