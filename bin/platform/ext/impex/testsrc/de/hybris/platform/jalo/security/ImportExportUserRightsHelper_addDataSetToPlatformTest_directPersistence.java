/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jalo.security;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.servicelayer.impex.ImportConfig;
import org.junit.Test;

@IntegrationTest
public class ImportExportUserRightsHelper_addDataSetToPlatformTest_directPersistence extends ImportExportUserRightsHelper_addDataSetToPlatformTest
{
    @Override
    protected ImportConfig getCompleteImpexConfig(ImpexRow... rows)
    {
        ImportConfig config = getStandardConfig();
        config.setScript(asResource(composeImpex(rows)));
        config.setSldForData(true);
        return config;
    }

    @Override
    protected String prefixed(String uid)
    {
        return "import_userrightshelper_sld_" + uid;
    }
}
