/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.audit.internal.conditional;

import static de.hybris.platform.persistence.audit.internal.conditional.ConditionalAuditTestUtils.ItemModelAuditRecordsAssert.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.directpersistence.cache.SLDDataContainerProvider;
import de.hybris.platform.persistence.audit.AuditChangeFilter;
import de.hybris.platform.persistence.audit.gateway.WriteAuditGateway;
import de.hybris.platform.persistence.audit.impl.DefaultAuditableSaver;
import de.hybris.platform.persistence.audit.internal.conditional.ConditionalAuditChangeFilter;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.type.TypeService;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.function.Consumer;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;


@IntegrationTest
public class ConditionalAuditTest extends ServicelayerBaseTest
{
	@Resource
	private DefaultAuditableSaver auditableSaver;

	@Resource
	private SLDDataContainerProvider sldDataContainerProvider;

	@Resource
	private TypeService typeService;

	@Resource
	private List<AuditChangeFilter> auditChangeFilters;

	@Resource
	private ModelService modelService;

	@Resource
	private WriteAuditGateway writeAuditGateway;

	@Before
	public void setUp() throws Exception
	{
	}

	@After
	public void restorePlatformAuditFilters()
	{
		auditableSaver.setAuditChangeFilters(auditChangeFilters);

		writeAuditGateway.removeAuditRecordsForType(UserModel._TYPECODE);
		writeAuditGateway.removeAuditRecordsForType(CustomerModel._TYPECODE);
	}

	@Test
	public void shouldNotAuditSuperType()
	{
		setupConditionalFilter("audit.test/conditional-audit-customer.xml");

		final CustomerModel auditedCustomer = createAuditedCustomer();
		final CustomerModel notAuditedCustomer = createCustomer();

		final UserModel auditedUser = createdAuditedUser();
		final UserModel notAuditedUser = createUser();

		assertThat(auditedCustomer).hasRecordedAudits(1);
		assertThat(notAuditedCustomer).hasRecordedAudits(0);

		assertThat(auditedUser).hasRecordedAudits(1);
		assertThat(notAuditedUser).hasRecordedAudits(1);
	}

	@Test
	public void shouldAuditSubtypesOfNotAuditedAbstractType()
	{
		setupConditionalFilter("audit.test/conditional-audit-principal.xml");

		final CustomerModel auditedCustomer = createAuditedCustomer();
		final CustomerModel notAuditedCustomer = createCustomer();

		final UserModel auditedUser = createdAuditedUser();
		final UserModel notAuditedUser = createUser();

		assertThat(auditedCustomer).hasRecordedAudits(1);
		assertThat(notAuditedCustomer).hasRecordedAudits(0);

		assertThat(auditedUser).hasRecordedAudits(1);
		assertThat(notAuditedUser).hasRecordedAudits(0);
	}

	@Test
	public void shouldOnlyAuditTypeIfSubtypesIsFalse()
	{
		setupConditionalFilter("audit.test/conditional-audit-user-no-subtypes.xml");

		final CustomerModel auditedCustomer = createAuditedCustomer();
		final CustomerModel notAuditedCustomer = createCustomer();

		final UserModel auditedUser = createdAuditedUser();
		final UserModel notAuditedUser = createUser();

		assertThat(auditedCustomer).hasRecordedAudits(1);
		assertThat(notAuditedCustomer).hasRecordedAudits(1);

		assertThat(auditedUser).hasRecordedAudits(1);
		assertThat(notAuditedUser).hasRecordedAudits(0);
	}


	@Test
	public void shouldAuditGenericItemSubtypes()
	{
		setupConditionalFilter("audit.test/conditional-audit-generic-item-only-march.xml");

		final CustomerModel auditedCustomer = createCustomerWith(i -> i.setCreationtime(getDateMatchingAuditCondition()));
		final CustomerModel notAuditedCustomer = createCustomerWith(i -> i.setCreationtime(getDateNotMatchingAuditCondition()));

		final TitleModel auditedTitle = createTitleWith(i -> i.setCreationtime(getDateMatchingAuditCondition()));
		final TitleModel notAuditedTitle = createTitleWith(i -> i.setCreationtime(getDateNotMatchingAuditCondition()));

		assertThat(auditedCustomer).hasRecordedAudits(1);
		assertThat(notAuditedCustomer).hasRecordedAudits(0);

		assertThat(auditedTitle).hasRecordedAudits(1);
		assertThat(notAuditedTitle).hasRecordedAudits(0);
	}

	private Date getDateMatchingAuditCondition()
	{
		final Instant marchInstant = LocalDate.of(2019, 3, 1).atStartOfDay().toInstant(ZoneOffset.UTC);
		return Date.from(marchInstant);
	}

	private Date getDateNotMatchingAuditCondition()
	{
		final Instant septemberInstant = LocalDate.of(2019, 9, 1).atStartOfDay().toInstant(ZoneOffset.UTC);
		return Date.from(septemberInstant);
	}

	private UserModel createdAuditedUser()
	{
		final UserModel auditedUser = modelService.create(UserModel.class);
		auditedUser.setUid(UUID.randomUUID().toString() + "_audit");
		modelService.save(auditedUser);
		return auditedUser;
	}

	private UserModel createUser()
	{
		final UserModel user = modelService.create(UserModel.class);
		user.setUid(UUID.randomUUID().toString());
		modelService.save(user);
		return user;
	}

	private CustomerModel createAuditedCustomer()
	{
		final CustomerModel auditedCustomer = modelService.create(CustomerModel.class);
		auditedCustomer.setUid(UUID.randomUUID().toString() + "_audit");
		modelService.save(auditedCustomer);
		return auditedCustomer;
	}

	private CustomerModel createCustomer()
	{
		return createCustomerWith(i -> {
		});
	}

	private CustomerModel createCustomerWith(final Consumer<CustomerModel> consumer)
	{
		final CustomerModel customer = modelService.create(CustomerModel.class);
		customer.setUid(UUID.randomUUID().toString());
		consumer.accept(customer);

		modelService.save(customer);
		return customer;
	}

	private TitleModel createTitleWith(final Consumer<TitleModel> consumer)
	{
		final TitleModel customer = modelService.create(TitleModel.class);
		customer.setCode(UUID.randomUUID().toString());
		consumer.accept(customer);

		modelService.save(customer);
		return customer;
	}

	private void setupConditionalFilter(final String configuration)
	{
		final ConditionalAuditChangeFilter auditChangeFilter = new ConditionalAuditChangeFilter(configuration,
				sldDataContainerProvider, typeService, Registry.getCurrentTenant().getConfig());

		auditableSaver.setAuditChangeFilters(List.of(auditChangeFilter));
	}
}
