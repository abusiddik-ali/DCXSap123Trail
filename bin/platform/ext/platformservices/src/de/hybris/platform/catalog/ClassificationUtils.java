/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.catalog;

import de.hybris.bootstrap.util.LocaleHelper;
import de.hybris.platform.catalog.model.classification.ClassAttributeAssignmentModel;


/**
 *
 */
public class ClassificationUtils
{
	/**
	 * creates a string with the pattern:
	 * <i>CatalogID/CatalogVersion/ClassificationClassCode.ClassificationAttributeCode</i></br> <b>OR</b> </br>
	 * <i>ClassificationClassCode.ClassificationAttributeCode</i> if system version
	 * {@link ClassAttributeAssignmentModel#SYSTEMVERSION} is null.
	 *
	 * @param classificationAttrAssignment the classification attribute assignment
	 * @return the qualifier created from the ClassificationAttributeAssignment attribute
	 */
	public static String createFeatureQualifier(final ClassAttributeAssignmentModel classificationAttrAssignment)
	{
		final StringBuilder builder = new StringBuilder(100);
		if (classificationAttrAssignment.getSystemVersion() != null)
		{
			builder.append(classificationAttrAssignment.getSystemVersion().getCatalog().getId()).append('/');
			builder.append(classificationAttrAssignment.getSystemVersion().getVersion()).append('/');
		}
		builder.append(classificationAttrAssignment.getClassificationClass().getCode()).append('.');
		builder.append(classificationAttrAssignment.getClassificationAttribute().getCode().toLowerCase(LocaleHelper.getPersistenceLocale()));
		return builder.toString();
	}
}
