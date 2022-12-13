/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.internal.jalo

import de.hybris.bootstrap.annotations.IntegrationTest
import de.hybris.platform.jalo.security.JaloSecurityException
import de.hybris.platform.servicelayer.ServicelayerBaseSpecification
import de.hybris.platform.testframework.PropertyConfigSwitcher
import de.hybris.platform.util.JspContext
import de.hybris.platform.util.logging.HybrisLogListener
import de.hybris.platform.util.logging.HybrisLogger
import org.apache.commons.lang3.RandomStringUtils
import org.apache.log4j.Level
import org.junit.Test
import spock.lang.Unroll

import java.util.function.Supplier

@IntegrationTest
class MediaUrlHashSaltSpec extends ServicelayerBaseSpecification {

    private final PropertyConfigSwitcher mediaHashSalt = new PropertyConfigSwitcher("media.default.storage.location.hash.salt")
    private final PropertyConfigSwitcher illegalValueBehaviour = new PropertyConfigSwitcher(
            "media.default.storage.location.hash.illegalValueBehavior")

    private ServicelayerManager servicelayerManager
    private HybrisLogListener listener

    @Override
    def setup() {
        servicelayerManager = ServicelayerManager.getInstance()

        listener = Mock(HybrisLogListener.class)
        listener.isEnabledFor(_ as Level) >> true

        HybrisLogger.addListener(listener)
    }

    @Override
    def cleanup() throws JaloSecurityException {
        mediaHashSalt.switchBackToDefault()
        illegalValueBehaviour.switchBackToDefault()

        HybrisLogger.removeListener(listener)
    }

    @Test
    @Unroll
    def "should #result.errorEffect when checking the #salt salt before #operation and behaviour is \"#error\""(Salt salt, Operation operation, String error, Result result) {

        given:
        illegalValueBehaviour.switchToValue(error)
        mediaHashSalt.switchToValue(salt.getSalt())

        when:
        def ex = null
        try {
            servicelayerManager.checkBeforeInitialization(JspContext.NULL_CONTEXT, operation == Operation.INIT)
        } catch (Exception e) {
            ex = e
        }

        then:
        result == Result.FAIL ? ex != null : ex == null

        where:
        salt         | operation        | error          | result
        Salt.DEFAULT | Operation.UPDATE | "error"        | Result.SUCCESS
        Salt.NULL    | Operation.UPDATE | "error"        | Result.SUCCESS
        Salt.RANDOM  | Operation.UPDATE | "error"        | Result.SUCCESS
        Salt.BLANK   | Operation.UPDATE | "error"        | Result.SUCCESS
        Salt.DEFAULT | Operation.INIT   | "error"        | Result.FAIL
        Salt.DEFAULT | Operation.INIT   | "Error"        | Result.FAIL
        Salt.DEFAULT | Operation.INIT   | "eRRor"        | Result.FAIL
        Salt.DEFAULT | Operation.INIT   | "ERROR"        | Result.FAIL
        Salt.DEFAULT | Operation.INIT   | "   error "    | Result.FAIL
        Salt.BLANK   | Operation.INIT   | "error"        | Result.FAIL
        Salt.RANDOM  | Operation.INIT   | "error"        | Result.SUCCESS
        Salt.DEFAULT | Operation.INIT   | "random value" | Result.SUCCESS

    }

    @Test
    @Unroll
    def "should #result.warningEffect when checking the #salt salt before #operation and behaviour is \"#warn\""(Salt salt, Operation operation, String warn, Result result) {

        given:
        illegalValueBehaviour.switchToValue(warn)
        mediaHashSalt.switchToValue(salt.getSalt())

        def expectedNumberOfLogs = result == Result.SUCCESS ? 0 : 1

        when:
        servicelayerManager.checkBeforeInitialization(JspContext.NULL_CONTEXT, operation == Operation.INIT)

        then:
        noExceptionThrown()
        expectedNumberOfLogs * listener.log({
            it.level == Level.WARN && it.message.contains("media.default.storage.location.hash.salt")
        })

        where:
        salt         | operation        | warn      | result
        Salt.DEFAULT | Operation.UPDATE | "warn"    | Result.SUCCESS
        Salt.NULL    | Operation.UPDATE | "warn"    | Result.SUCCESS
        Salt.RANDOM  | Operation.UPDATE | "warn"    | Result.SUCCESS
        Salt.BLANK   | Operation.UPDATE | "warn"    | Result.SUCCESS
        Salt.DEFAULT | Operation.INIT   | "warn"    | Result.FAIL
        Salt.DEFAULT | Operation.INIT   | "WARN"    | Result.FAIL
        Salt.DEFAULT | Operation.INIT   | "wArn"    | Result.FAIL
        Salt.DEFAULT | Operation.INIT   | "  warn " | Result.FAIL
        Salt.BLANK   | Operation.INIT   | "warn"    | Result.FAIL
        Salt.RANDOM  | Operation.INIT   | "warn"    | Result.SUCCESS
        Salt.RANDOM  | Operation.INIT   | "WARN"    | Result.SUCCESS
        Salt.RANDOM  | Operation.INIT   | "wArn"    | Result.SUCCESS
        Salt.RANDOM  | Operation.INIT   | "  warn " | Result.SUCCESS
        Salt.DEFAULT | Operation.INIT   | ""        | Result.FAIL
    }

    @Test
    @Unroll
    def "should have no effect when checking the #salt salt before #operation and behaviour is \"#info\""(Salt salt, Operation operation, String info) {

        given:
        illegalValueBehaviour.switchToValue(info)
        mediaHashSalt.switchToValue(salt.getSalt())

        when:
        servicelayerManager.checkBeforeInitialization(JspContext.NULL_CONTEXT, operation == Operation.INIT)

        then:
        noExceptionThrown()
        0 * listener.log({
            it.level == Level.WARN && it.message.contains("media.default.storage.location.hash.salt")
        })

        where:
        salt         | operation        | info
        Salt.DEFAULT | Operation.UPDATE | "info"
        Salt.NULL    | Operation.UPDATE | "info"
        Salt.RANDOM  | Operation.UPDATE | "info"
        Salt.BLANK   | Operation.UPDATE | "info"
        Salt.DEFAULT | Operation.INIT   | "info"
        Salt.DEFAULT | Operation.INIT   | "iNFO"
        Salt.DEFAULT | Operation.INIT   | "InFo"
        Salt.DEFAULT | Operation.INIT   | " info  "
        Salt.NULL    | Operation.INIT   | "info"
        Salt.BLANK   | Operation.INIT   | "info"
        Salt.RANDOM  | Operation.INIT   | "info"
    }

    enum Salt {
        DEFAULT("default", { "35b5cd0da3121fc53b4bc84d0c8af2e81" }),
        RANDOM("random", { RandomStringUtils.randomAlphabetic(10) }),
        NULL("null", { null }),
        BLANK("blank", { "    " });

        private final Supplier<String> saltSupplier;
        private final String code;

        Salt(final String code, final Supplier<String> saltSupplier) {
            this.saltSupplier = saltSupplier
            this.code = code
        }

        String getCode() {
            return code
        }

        String getSalt() { saltSupplier.get() }
    }

    enum Operation {
        INIT, UPDATE
    }

    enum Result {
        SUCCESS("don't produce logs", "pass "), FAIL("produce logs", "throw exception")

        String warningEffect;
        String errorEffect;

        Result(final String warningEffect, final String errorEffect) {
            this.warningEffect = warningEffect
            this.errorEffect = errorEffect
        }
    }
}
