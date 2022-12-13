/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.task.utils;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


/**
 * Marker annotation which let's test framework know, that this test needs a running task engine. <br/>
 * For tests marked with this annotation, test framework will automatically start task engine before, and stop after the
 * test.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(
		{ ElementType.TYPE })
public @interface NeedsTaskEngine
{
}
