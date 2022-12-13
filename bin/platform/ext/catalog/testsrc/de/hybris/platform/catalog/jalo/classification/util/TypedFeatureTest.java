/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog.jalo.classification.util;

import de.hybris.bootstrap.annotations.UnitTest;
import org.junit.Test;

import static de.hybris.platform.testframework.Assert.assertEquals;

@UnitTest
public class TypedFeatureTest
{

    @Test
    public void testToEnumAndBack()
    {
        assertEquals(TypedFeature.FeatureType.toEnum(TypedFeature.FeatureType.STRING.toEnumValue()), TypedFeature.FeatureType.STRING);
        assertEquals(TypedFeature.FeatureType.toEnum(TypedFeature.FeatureType.NUMBER.toEnumValue()), TypedFeature.FeatureType.NUMBER);
        assertEquals(TypedFeature.FeatureType.toEnum(TypedFeature.FeatureType.BOOLEAN.toEnumValue()), TypedFeature.FeatureType.BOOLEAN);
        assertEquals(TypedFeature.FeatureType.toEnum(TypedFeature.FeatureType.ENUM.toEnumValue()), TypedFeature.FeatureType.ENUM);
        assertEquals(TypedFeature.FeatureType.toEnum(TypedFeature.FeatureType.DATE.toEnumValue()), TypedFeature.FeatureType.DATE);
        assertEquals(TypedFeature.FeatureType.toEnum(TypedFeature.FeatureType.REFERENCE.toEnumValue()), TypedFeature.FeatureType.REFERENCE);
    }
}
