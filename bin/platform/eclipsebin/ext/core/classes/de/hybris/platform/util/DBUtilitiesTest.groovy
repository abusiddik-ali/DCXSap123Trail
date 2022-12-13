/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.util

import de.hybris.bootstrap.annotations.UnitTest
import org.junit.Test
import spock.lang.Shared
import spock.lang.Specification
import spock.lang.Unroll

@UnitTest
class DBUtilitiesTest extends Specification {

    @Shared
    private DBUtilities dbUtilities = new DBUtilities()

    @Test
    @Unroll
    def "ApplySpecialPropertiesToDatabaseUrl"(final String input, final String expected) {
        when:
        String result = dbUtilities.applySpecialPropertiesToDatabaseUrl(input)

        then:
        result == expected


        where:
        input                                                           | expected
        "jdbc:sap://hana.com:1234/?reconnect=true&cache=256"            |
                "jdbc:sap://hana.com:1234/?reconnect=true&cache=256&SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA"

        "jdbc:sap://hana.com:1234"                                      |
                "jdbc:sap://hana.com:1234?SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA"

        "jdbc:sap://hana.com:1234/?SESSIONVARIABLE:APPLICATION1=SAP_COMMERCE_HRA" |
                "jdbc:sap://hana.com:1234/?SESSIONVARIABLE:APPLICATION1=SAP_COMMERCE_HRA&SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA"

        "jdbc:sap://hana.com:1234/?SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA" |
                "jdbc:sap://hana.com:1234/?SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA"

        "jdbc:sap://hana.com:1234/?SESSIONVARIABLE:APPLICATION=UNKNOWN" |
                "jdbc:sap://hana.com:1234/?SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA"

        "jdbc:sap://hana.com:1234/?SESSIONVARIABLE:APPLICATION="        |
                "jdbc:sap://hana.com:1234/?SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA"

        "jdbc:sap://hana.com:1234/?SESSIONVARIABLE:APPLICATION=UNKNOWN&cache=256" |
                "jdbc:sap://hana.com:1234/?cache=256&SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA"

        "jdbc:sap://hana.com:1234/?reconnect=true&SESSIONVARIABLE:APPLICATION=UNKNOWN&cache=256" |
                "jdbc:sap://hana.com:1234/?reconnect=true&cache=256&SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA"

        "jdbc:sap://hana.com:1234/?111SESSIONVARIABLE:APPLICATION=UNKNOWN" |
                "jdbc:sap://hana.com:1234/?111SESSIONVARIABLE:APPLICATION=UNKNOWN&SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA"

        "jdbc:sap://hana.com:1234/?SESSIONVARIABLE:APPLICATION=UNKNOWN1&SESSIONVARIABLE:APPLICATION=UNKNOWN2" |
                "jdbc:sap://hana.com:1234/?SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA"

        "jdbc:sap://hana.com:1234/?SESSIONVARIABLE:APPLICATION=UNKNOWN1&reconnect=true&SESSIONVARIABLE:APPLICATION=UNKNOWN2" |
                "jdbc:sap://hana.com:1234/?reconnect=true&SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA"

        "jdbc:sap://hana.com:1234?sessionVariable:AppliCATion=SAP_COMMERCE_HRA" |
                "jdbc:sap://hana.com:1234?SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA"

        "jdbc:sap://hana.com:1234?reconnect=true&sessionVariable:AppliCATion=SAP_COMMERCE_HRA" |
                "jdbc:sap://hana.com:1234?reconnect=true&SESSIONVARIABLE:APPLICATION=SAP_COMMERCE_HRA"

        //mysql
        "jdbc:mysql://mysql.com/hyb"                               | "jdbc:mysql://mysql.com/hyb?nullCatalogMeansCurrent=true"
        "jdbc:mysql://mysql.com/hyb?param=true"                    | "jdbc:mysql://mysql.com/hyb?param=true&nullCatalogMeansCurrent=true"
        "jdbc:mysql://mysql.com/hyb?param=true&param1=false"       | "jdbc:mysql://mysql.com/hyb?param=true&param1=false&nullCatalogMeansCurrent=true"
        "jdbc:mysql://mysql.com/hyb?nullCatalogMeansCurrent=false" | "jdbc:mysql://mysql.com/hyb?nullCatalogMeansCurrent=false"

        //other databases
        "jdbc:sqlserver://sql.com:1234"                                 | "jdbc:sqlserver://sql.com:1234"
        "jdbc:postgresql://postgresql:1234/hybris"                      | "jdbc:postgresql://postgresql:1234/hybris"
        "jdbc:oracle:thin:@//oracle.com:1234"                           | "jdbc:oracle:thin:@//oracle.com:1234"
    }
}
