/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test;

/**
 * Factory to create specific logic for each worker.
 */
public interface RunnerCreator<T extends Runnable>
{
	/**
	 * Creates a new runnable object for the given runner thread number.
	 */
	T newRunner(int threadNumber);
}