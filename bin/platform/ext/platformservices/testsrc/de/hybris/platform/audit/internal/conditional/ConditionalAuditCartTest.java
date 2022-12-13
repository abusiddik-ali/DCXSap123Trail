/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.audit.internal.conditional;

import static de.hybris.platform.persistence.audit.internal.conditional.ConditionalAuditTestUtils.ItemModelAuditRecordsAssert.assertThat;
import static de.hybris.platform.persistence.audit.internal.conditional.ConditionalAuditTestUtils.getAuditRecordsFor;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.directpersistence.cache.SLDDataContainerProvider;
import de.hybris.platform.order.CartService;
import de.hybris.platform.persistence.audit.AuditChangeFilter;
import de.hybris.platform.persistence.audit.gateway.AuditRecord;
import de.hybris.platform.persistence.audit.gateway.WriteAuditGateway;
import de.hybris.platform.persistence.audit.impl.DefaultAuditableSaver;
import de.hybris.platform.persistence.audit.internal.conditional.ConditionalAuditChangeFilter;
import de.hybris.platform.product.ProductService;
import de.hybris.platform.product.daos.UnitDao;
import de.hybris.platform.servicelayer.ServicelayerTest;
import de.hybris.platform.servicelayer.i18n.daos.CurrencyDao;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.type.TypeService;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.annotation.Resource;

import org.apache.commons.io.IOUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Iterables;


@IntegrationTest
public class ConditionalAuditCartTest extends ServicelayerTest
{
	private static final String CONDITIONAL_AUDIT_CART_CONF = "audit.test/conditional-audit-cart-entry.xml";

	@Resource
	private ProductService productService;

	@Resource
	private CartService cartService;

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
	CurrencyDao currencyDao;

	@Resource
	UnitDao unitDao;

	@Resource
	WriteAuditGateway writeAuditGateway;

	private ProductModel product0;
	private ProductModel product1;
	private ProductModel product2;

	@Before
	public void setUp() throws Exception
	{
		createCoreData();
		createDefaultCatalog();
		getOrCreateCurrency("en");

		product0 = productService.getProductForCode("testProduct0");
		product1 = productService.getProductForCode("testProduct1");
		product2 = productService.getProductForCode("testProduct2");
	}

	@After
	public void restorePlatformAuditFilters()
	{
		auditableSaver.setAuditChangeFilters(auditChangeFilters);

		writeAuditGateway.removeAuditRecordsForType(CartModel._TYPECODE);
		writeAuditGateway.removeAuditRecordsForType(CartEntryModel._TYPECODE);
	}


	@Test
	public void shouldRecordAuditWhenMatchingConfig()
	{
		setupConditionalFilter(CONDITIONAL_AUDIT_CART_CONF);

		cartService.removeSessionCart();
		final CartModel auditedCart = cartService.getSessionCart();
		final CartAuditRecorder recorder = new CartAuditRecorder(auditedCart);

		assertThat(recorder.newAuditRecorded()).isFalse();

		auditedCart.setStatusInfo("audited-cart");
		modelService.save(auditedCart);

		assertThat(recorder.newAuditRecorded()).isTrue();

		final CartEntryModel product0Entry = cartService.addNewEntry(auditedCart, product0, 2, null);
		modelService.saveAll();

		assertThat(recorder.newAuditRecorded()).isTrue();
		assertThat(recorder.newAuditRecorded()).isFalse();

		final CartEntryModel product1Entry = cartService.addNewEntry(auditedCart, product1, 1, null);
		cartService.addNewEntry(auditedCart, product2, 1, null);
		modelService.saveAll();

		product0Entry.setQuantity(10L);
		modelService.saveAll();

		assertThat(recorder.newAuditRecorded()).isTrue();

		modelService.remove(product1Entry);

		assertThat(recorder.newAuditRecorded()).isTrue();
		assertThat(recorder.noAuditRecorded()).isFalse();
	}

	@Test
	public void shouldNotRecordAuditWhenNotMatchingConfig()
	{
		setupConditionalFilter(CONDITIONAL_AUDIT_CART_CONF);

		cartService.removeSessionCart();
		final CartModel notAuditedCart = cartService.getSessionCart();

		final CartAuditRecorder recorder = new CartAuditRecorder(notAuditedCart);

		assertThat(recorder.newAuditRecorded()).isFalse();

		modelService.save(notAuditedCart);

		assertThat(recorder.newAuditRecorded()).isFalse();

		cartService.addNewEntry(notAuditedCart, product0, 2, null);
		modelService.saveAll();

		assertThat(recorder.newAuditRecorded()).isFalse();

		cartService.addNewEntry(notAuditedCart, product1, 1, null);
		cartService.addNewEntry(notAuditedCart, product2, 1, null);
		modelService.saveAll();

		assertThat(recorder.newAuditRecorded()).isFalse();
	}


	@Test
	public void shouldAuditCartBecauseConditionalIsFalse()
	{
		setupFilterIgnoringAuditWithConditionalAttributes(true, false, true);

		final UserModel user = createUser();
		final CartModel cart = createCart(user);
		final CartEntryModel cartEntry = createCartEntry(cart);

		assertThat(user).hasRecordedAudits(0);
		assertThat(cart).hasRecordedAudits(1);
		assertThat(cartEntry).hasRecordedAudits(0);
	}

	@Test
	public void shouldAuditUserAndCartBecauseConditionalIsFalse()
	{
		setupFilterIgnoringAuditWithConditionalAttributes(false, false, true);

		final UserModel user = createUser();
		final CartModel cart = createCart(user);
		final CartEntryModel cartEntry = createCartEntry(cart);

		assertThat(user).hasRecordedAudits(1);
		assertThat(cart).hasRecordedAudits(1);
		assertThat(cartEntry).hasRecordedAudits(0);
	}

	@Test
	public void shouldAuditUserBecauseConditionalIsFalse()
	{
		setupFilterIgnoringAuditWithConditionalAttributes(false, true, true);

		final UserModel user = createUser();
		final CartModel cart = createCart(user);
		final CartEntryModel cartEntry = createCartEntry(cart);

		assertThat(user).hasRecordedAudits(1);
		assertThat(cart).hasRecordedAudits(0);
		assertThat(cartEntry).hasRecordedAudits(0);
	}

	private static class CartAuditRecorder
	{
		private final CartModel cart;

		private final List<CartAudit> audits = new ArrayList<>();

		private CartAuditRecorder(final CartModel cart)
		{
			this.cart = cart;
		}

		public boolean newAuditRecorded()
		{
			final CartAudit currentAudits = getAuditRecordsForCart(cart);

			if (audits.isEmpty())
			{
				audits.add(currentAudits);
				return currentAudits.recordedAuditsNumber() > 0;
			}

			final CartAudit lastAudit = Iterables.getLast(audits);
			audits.add(currentAudits);

			return currentAudits.recordedAuditsNumber() > lastAudit.recordedAuditsNumber();
		}

		public boolean noAuditRecorded()
		{
			return audits.stream().allMatch(i -> i.noAuditRecorded());
		}

	}

	public static class CartAudit
	{
		List<AuditRecord> cartAudit;
		Map<ProductModel, List<AuditRecord>> productEntryAudit = new HashMap<>();

		boolean noAuditRecorded()
		{
			return !recordedAudit();
		}

		boolean recordedAudit()
		{
			if (cartAudit != null && !cartAudit.isEmpty())
			{
				return true;
			}

			return productEntryAudit.values().stream().anyMatch(i -> !i.isEmpty());
		}

		int recordedAuditsNumber()
		{
			int counter = cartAudit != null ? cartAudit.size() : 0;

			for (final List<AuditRecord> audits : productEntryAudit.values())
			{
				counter += audits.size();
			}

			return counter;
		}
	}


	public static CartAudit getAuditRecordsForCart(final CartModel cart)
	{
		final CartAudit result = new CartAudit();
		result.cartAudit = getAuditRecordsFor(CartModel._TYPECODE, cart.getPk());

		for (final AbstractOrderEntryModel entry : cart.getEntries())
		{
			final List<AuditRecord> entryAudit = getAuditRecordsFor(CartEntryModel._TYPECODE, entry.getPk());
			result.productEntryAudit.put(entry.getProduct(), entryAudit);
		}

		return result;
	}

	private UserModel createUser()
	{
		final UserModel user = modelService.create(UserModel.class);
		user.setUid(UUID.randomUUID().toString());
		modelService.save(user);
		return user;
	}

	private CartModel createCart(final UserModel user)
	{
		final CartModel cart = modelService.create(CartModel.class);
		cart.setUser(user);
		cart.setDate(new Date());
		cart.setCurrency(currencyDao.findCurrenciesByCode("en").get(0));
		modelService.save(cart);
		return cart;
	}

	private CartEntryModel createCartEntry(final CartModel cart)
	{
		final CartEntryModel cartEntry = modelService.create(CartEntryModel.class);
		cartEntry.setOrder(cart);
		cartEntry.setProduct(product0);
		cartEntry.setQuantity(1L);
		cartEntry.setUnit(unitDao.findUnitsByCode("pieces").iterator().next());
		modelService.save(cartEntry);
		return cartEntry;
	}

	private void setupConditionalFilter(final String configuration)
	{
		final ConditionalAuditChangeFilter filter = new ConditionalAuditChangeFilter(configuration,
				sldDataContainerProvider, typeService,
				Registry.getCurrentTenant().getConfig());
		auditableSaver.setAuditChangeFilters(List.of(filter));
	}

	private void setupFilterIgnoringAuditWithConditionalAttributes(final boolean userConditional, final boolean cartConditional,
	                                                               final Boolean cartEntryConditional)
	{
		final ConditionalAuditChangeFilter conditionalConfiguration = getConditionalConfiguration(userConditional,
				cartConditional, cartEntryConditional);
		auditableSaver.setAuditChangeFilters(List.of(conditionalConfiguration));
	}

	private ConditionalAuditChangeFilter getConditionalConfiguration(final Boolean userConditional, final Boolean cartConditional,
	                                                                 final Boolean cartEntryConditional)
	{
		final String configurationFromTemplate = getConfigurationFromTemplate(userConditional, cartConditional,
				cartEntryConditional);

		return ConditionalAuditChangeFilter.fromConfigText(configurationFromTemplate, sldDataContainerProvider, typeService,
				Registry.getCurrentTenant().getConfig());
	}

	private String getConfigurationFromTemplate(final Boolean userConditional, final Boolean cartConditional,
	                                            final Boolean cartEntryConditional)
	{
		try (final InputStream resourceAsStream = getClass().getClassLoader()
		                                                    .getResourceAsStream(
				                                                    "audit.test/conditional-audit-user-cart-entry-template.xml"))
		{
			final String configurationTemplate = IOUtils.toString(resourceAsStream, Charset.defaultCharset());
			final String formattedConfiguration = String.format(configurationTemplate, userConditional.toString(),
					cartConditional.toString(),
					cartEntryConditional.toString());

			return formattedConfiguration;
		}
		catch (final IOException e)
		{
			throw new RuntimeException(e);
		}
	}

}
