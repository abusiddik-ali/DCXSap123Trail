/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer

import de.hybris.platform.testframework.HybrisSpockSpecification
import org.springframework.context.ApplicationContext

import javax.annotation.Resource
import java.lang.reflect.Field

class ServicelayerBaseSpecification extends HybrisSpockSpecification {
    protected static final ServicelayerBaseTestLogic servicelayerBaseTestLogic = new ServicelayerBaseTestLogic();

    def setup() {
        println("test setup: prepareApplicationContextAndSession")
        servicelayerBaseTestLogic.prepareApplicationContextAndSession(this)
    }

    protected void autowireProperties(final ApplicationContext applicationContext) {
        servicelayerBaseTestLogic.autowireProperties(applicationContext, this)
    }

    protected String getBeanName(final Resource resource, final Field field) {
        return servicelayerBaseTestLogic.getBeanName(resource, field)
    }

    protected ApplicationContext getApplicationContext() {
        return servicelayerBaseTestLogic.getApplicationContext()
    }
}
