/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.persistence.flexiblesearch.polyglot

import de.hybris.bootstrap.annotations.IntegrationTest
import de.hybris.platform.jalo.flexiblesearch.FlexibleSearchException
import de.hybris.platform.jalo.flexiblesearch.QueryOptions
import de.hybris.platform.servicelayer.ServicelayerBaseSpecification
import org.antlr.v4.runtime.misc.ParseCancellationException
import org.junit.Test
import spock.lang.Unroll

@IntegrationTest
class PolyglotQueryCriteriaConversionSpecification extends ServicelayerBaseSpecification {

    @Unroll
    @Test
    def "check valid polyglot query"() {
        when:
        PolyglotPersistenceFlexibleSearchSupport.tryToConvertToPolyglotCriteria(QueryOptions.newBuilder().withQuery(query).withValues(params).build())

        then:
        noExceptionThrown()

        where:
        query                                                                   | params
        "GET {Title} WHERE {A}=?a AND ( ( {B}=?b OR {C}= ? c ) OR {D}=?d )"     | ["a": "a", "b": "b", "c": "c", "d": "d"]
        "GET{Title}WHERE{A}=?a AND(({B}=?b OR {C}=?c) OR {D}=?d)"               | ["a": "a", "b": "b", "c": "c", "d": "d"]
        "GeT {Title} wHeRe {A}=?a aNd ( ( {B}=?b Or {C}= ? c ) oR {D}=?d )"     | ["a": "a", "b": "b", "c": "c", "d": "d"]
        "GET {Title} WHERE {AA}=?aa AND ( {DD}=?dd )"                           | ["aa": "aa", "dd": "dd"]
        "GET {Title} WHERE ((({A}=?a)))"                                        | ["a": "a"]
        "GET {Title} WHERE {A}=?a AND {B}=?b"                                   | ["a": "a", "b": "b"]
        "GET {Title} WHERE {A}=?a AND {B}=?b OR {C}= ? c OR {D}=?d"             | ["a": "a", "b": "b", "c": "c", "d": "d"]
        "GET {Title} WHERE {A} IS NULL"                                         | [:]
        "GET {Title} WHERE {A}IS    NULL"                                       | [:]
        "GET {Title} WHERE {A} IS NOT NULL"                                     | [:]
        "GET {Title} WHERE {A} IS NULL OR {B} IS NOT NULL"                      | [:]
        "GET {Title} WHERE {A} IS NOT NULL AND {B} IS NULL"                     | [:]
        "GET {Title} WHERE {A} iS nOt NulL"                                     | [:]
        "GET {Title} WHERE {A} IS NULL AND {A} IS NULL"                         | [:]
        "GET {Title} WHERE {A}=?a OR {B} IS NULL"                               | ["a": "a"]
        "GET {Title} WHERE {A}=?a AND {B} IS NOT NULL"                          | ["a": "a"]
        "GET {Title} WHERE {A} IS NOT NULL OR {B}=?b"                           | ["b": "b"]
        "GET {Title} WHERE {A} IS NULL AND {B}=?b"                              | ["b": "b"]
        "GET {Title} WHERE (({A} IS NOT NULL OR {B}=?b) AND (({C} IS NULL)))"   | ["b": "b"]
        "GET {Title} WHERE ((({A} IS NULL)) OR ({B}=?b AND ({C} IS NOT NULL)))" | ["b": "b"]
        "GET {Title} WHERE ((({A} IS NULL)))"                                   | [:]
    }

    @Unroll
    def "check query with invalid type"() {
        when:
        PolyglotPersistenceFlexibleSearchSupport.tryToConvertToPolyglotCriteria(QueryOptions.newBuilder().withQuery(query).withValues(params).build())

        then:
        thrown(FlexibleSearchException)

        where:
        query                                                                      | params
        "GET {aNyOtHeRtYpE} WHERE {A}=?a AND ( ( {B}=?b OR {C}= ? c ) OR {D}=?d )" | ["a": "a", "b": "b", "c": "c", "d": "d"]
    }

    @Unroll
    def "check invalid polyglot query"() {
        when:
        PolyglotPersistenceFlexibleSearchSupport.tryToConvertToPolyglotCriteria(QueryOptions.newBuilder().withQuery(query).withValues(params).build())

        then:
        thrown(ParseCancellationException)

        where:
        query                                              | params
        "GET {} WHERE {A}=?a"                              | ["a": "a"]
        "GET {Title} {A}=?a"                               | ["a": "a"]
        "GET {Title} WHERE AND ( ( {B}=?b OR {C}= ? c ) )" | ["a": "a", "b": "b", "c": "c"]
        "GET {Title} WHERE ( ( {B}=?b  ) "                 | ["a": "a", "b": "b", "c": "c"]
        "GET {Title} WHERE {A}=?a AND "                    | ["a": "a", "b": "b", "c": "c"]
        "GET {Title} WHERE {A}=?a OR"                      | ["a": "a", "b": "b", "c": "c"]
        "GET {Title} WHERE ()"                             | ["a": "a", "b": "b", "c": "c"]
        "GET {Title} where "                               | ["a": "a", "b": "b", "c": "c"]
        "GET {Title} WHERE A=?a"                           | ["a": "a", "b": "b", "c": "c"]
        "GET {Title} WHERE {A}=?{a}"                       | ["a": "a", "b": "b", "c": "c"]
        "GET {Title} WHERE {A=?a}"                         | ["a": "a", "b": "b", "c": "c"]
        "GET {Title} WHERE (A)=?a"                         | ["a": "a", "b": "b", "c": "c"]
        "GET {Title} WHERE {A}= "                          | ["a": "a"]
        "GET {Title} WHERE {A}=? "                         | ["a": "a"]
        "GET {Title} WHERE {A}=? OR {B}=?b"                | ["a": "a", "b": "b"]
        "GET {Title} WHERE {A}=? AND {B}=?b"               | ["a": "a", "b": "b"]
        "GET {Title} WHERE {A}= OR {B}=?b"                 | ["a": "a", "b": "b"]
        "GET {Title} WHERE {A}= AND {B}=?b"                | ["a": "a", "b": "b"]
        "GET {Title} WHERE {A} S NOT NULL"                 | [:]
        "GET {Title} WHERE {A} IS OT NULL"                 | [:]
        "GET {Title} WHERE {A} IS NT NULL"                 | [:]
        "GET {Title} WHERE {A} IS NOT NUL"                 | [:]
        "GET {Title} WHERE {A} NOT NULL"                   | [:]
        "GET {Title} WHERE {A} IS NOT"                     | [:]
        "GET {Title} WHERE {A} I NUL"                      | [:]
        "GET {Title} WHERE {A} I NULL"                     | [:]
        "GET {Title} WHERE (A) IS NULL"                    | [:]
        "GET {Title!} WHERE {A} IS NULL"                   | [:]
        "GET {Title@} WHERE {A} IS NULL"                   | [:]
        "GET {Title#} WHERE {A} IS NULL"                   | [:]
        "GET {Title\$} WHERE {A} IS NULL"                  | [:]
        "GET {Title%} WHERE {A} IS NULL"                   | [:]
        "GET {Title^} WHERE {A} IS NULL"                   | [:]
        "GET {Title&} WHERE {A} IS NULL"                   | [:]
        "GET {Title*} WHERE {A} IS NULL"                   | [:]
        "GET {Title+} WHERE {A} IS NULL"                   | [:]
        "GET {Title-} WHERE {A} IS NULL"                   | [:]
        "GET {Title} WHERE {A+} IS NULL"                   | [:]
        "GET {Title} WHERE {A*} IS NULL"                   | [:]
        "GET {Title} WHERE {A!} IS NULL"                   | [:]
    }

    @Unroll
    def "check no polyglot query returns null"() {
        expect:
        PolyglotPersistenceFlexibleSearchSupport.tryToConvertToPolyglotCriteria(QueryOptions.newBuilder().withQuery(query).withValues(params).build()) == Optional.empty()

        where:
        query                                                        | params
        " {} WHERE {A}=?a"                                           | ["a": "a"]
        "SELECT {Title} {A}=?a"                                      | ["a": "a"]
        "select * from {Title} WHERE AND ( ( {B}=?b OR {C}= ? c ) )" | ["b": "b", "c": "c"]
        "SomethingElse {Title} WHERE ( ( {B}=?b  ) "                 | ["b": "b"]
    }
}
