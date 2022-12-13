/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation, which indicates whether existing Spring context should be enriched with configuration defined in given
 * XML resources. File can define new beans or redefine existing ones. Test class which is annotated by this type should
 * extend ServicelayerBaseSpecification class, but most commonly ServicelayerTransactionalSpockSpecification class. Example usage:
 *
 * <pre>
 *     &#64;AppendSpringConfiguration(["test-spring.xml"])
 * </pre>
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface AppendSpringConfiguration
{
	/**
	 * Returns array of XML resource names, which should be added to Spring configuration
	 *
	 * @return array of XML resource names, which should be added to Spring configuration
	 */
	String[] value();
}
