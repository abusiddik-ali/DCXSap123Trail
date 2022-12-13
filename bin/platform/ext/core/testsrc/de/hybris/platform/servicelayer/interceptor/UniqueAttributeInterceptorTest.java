/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.interceptor;

import static org.apache.commons.lang3.exception.ExceptionUtils.getRootCause;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.assertj.core.api.Assertions.failBecauseExceptionWasNotThrown;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.catalog.model.CatalogModel;
import de.hybris.platform.catalog.model.CatalogVersionModel;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.core.model.product.UnitModel;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.jalo.ConsistencyCheckException;
import de.hybris.platform.jalo.CoreBasicDataCreator;
import de.hybris.platform.jalo.flexiblesearch.FlexibleSearch;
import de.hybris.platform.jalo.flexiblesearch.FlexibleSearchException;
import de.hybris.platform.jalo.product.Unit;
import de.hybris.platform.jalo.type.AttributeDescriptor;
import de.hybris.platform.jalo.type.TypeManager;
import de.hybris.platform.jalo.user.Title;
import de.hybris.platform.jalo.user.User;
import de.hybris.platform.jalo.user.UserManager;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.servicelayer.i18n.L10NService;
import de.hybris.platform.servicelayer.interceptor.impl.UniqueAttributesInterceptor;
import de.hybris.platform.servicelayer.interceptor.impl.UniqueAttributesInterceptor.AmbiguousUniqueKeysException;
import de.hybris.platform.servicelayer.interceptor.impl.UniqueAttributesInterceptor.NonSearchableAttributeException;
import de.hybris.platform.servicelayer.internal.converter.impl.ItemModelConverter;
import de.hybris.platform.servicelayer.internal.model.impl.DefaultModelService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionExecutionBody;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.testframework.TestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;


// This test cannot be transactional since it depends on ModeConverter meta data caches are updated which happens
// only on real invalidations (in tx only at commit which would be too late )
@IntegrationTest
public class UniqueAttributeInterceptorTest extends ServicelayerBaseTest
{

	private static String UNIQUE_MANY_MSG_EN;
	private static String UNIQUE_AT_LEASTONE_MSG_EN;
	private static String UNIQUE_NONSEARCHABLE__MSG_EN;


	private static String UNIQUE_MANY_MSG_DE;
	private static String UNIQUE_AT_LEASTONE_MSG_DE;
	private static String UNIQUE_NONSEARCHABLE__MSG_DE;


	private boolean unitCodeIsUniqueBefore;
	private boolean titleCodeIsSearchableBefore;
	private boolean unitTypeIsUniqueBefore;

	@Resource
	private ModelService modelService;

	@Resource
	private I18NService i18nService;

	@Resource
	private UserService userService;


	@Resource
	private SessionService sessionService;

	@Resource
	private L10NService l10nService;

	@Before
	public void setUp()
	{
		new CoreBasicDataCreator().createBasicC2L();

		prepareMessages();

		final ItemModelConverter unitConv = (ItemModelConverter) ((DefaultModelService) modelService).getConverterRegistry()
		                                                                                             .getModelConverterByModelType(
				                                                                                             UnitModel.class);
		final Set<String> uniqueAttributesBefore = unitConv.getUniqueAttributes();

		final AttributeDescriptor codeAttr = TypeManager.getInstance().getComposedType(Unit.class)
		                                                .getAttributeDescriptor(Unit.CODE);
		unitCodeIsUniqueBefore = codeAttr.isUnique();

		codeAttr.setUnique(true);


		final AttributeDescriptor titleCodeAttr = TypeManager.getInstance().getComposedType(Title.class)
		                                                     .getAttributeDescriptor(Title.CODE);
		titleCodeIsSearchableBefore = titleCodeAttr.isSearchable();
		titleCodeAttr.setSearchable(false);

		final AttributeDescriptor typeAttr = TypeManager.getInstance().getComposedType(Unit.class)
		                                                .getAttributeDescriptor(Unit.UNITTYPE);
		unitTypeIsUniqueBefore = typeAttr.isUnique();
		typeAttr.setUnique(true);

		final Set<String> expectedUniqueAfter = new HashSet<>(uniqueAttributesBefore);
		expectedUniqueAfter.add(Unit.UNITTYPE);

		final Set<String> uniqueAttributesAfter = unitConv.getUniqueAttributes();
		assertThat(uniqueAttributesAfter).isEqualTo(expectedUniqueAfter);
	}

	private void prepareMessages()
	{
		final String messageKeyPrefix = "exception." + UniqueAttributesInterceptor.class.getSimpleName().toLowerCase(Locale.ENGLISH);
		final String[] messagesEn = prepareMessageForLocale(messageKeyPrefix, Locale.ENGLISH);

		UNIQUE_MANY_MSG_EN = messagesEn[0];
		UNIQUE_AT_LEASTONE_MSG_EN = messagesEn[1];
		UNIQUE_NONSEARCHABLE__MSG_EN = messagesEn[2];

		final String[] messagesDe = prepareMessageForLocale(messageKeyPrefix, Locale.GERMAN);


		UNIQUE_MANY_MSG_DE = messagesDe[0];
		UNIQUE_AT_LEASTONE_MSG_DE = messagesDe[1];
		UNIQUE_NONSEARCHABLE__MSG_DE = messagesDe[2];

	}

	private String[] prepareMessageForLocale(final String messageKeyPrefix, final Locale locale)
	{
		final String[] messages = sessionService.executeInLocalView(new SessionExecutionBody()
		{
			@Override
			public Object execute()
			{
				i18nService.setCurrentLocale(locale);


				final Map<String, Object> map = new LinkedHashMap<String, Object>();
				map.put("unitType", "type");
				map.put("code", "u3");

				return new String[]
						{ l10nService.getLocalizedString(messageKeyPrefix + ".uniquemany", new Object[]
								{ map, new UnitModel(), "1" }),//
								l10nService.getLocalizedString(messageKeyPrefix + ".uniqueatleatone", new Object[]
										{ map, new UnitModel() }), //
								l10nService.getLocalizedString(messageKeyPrefix + ".searchable", new Object[]
										{ "code", "Title" }) };
			}
		});
		return messages;
	}

	@After
	public void tearDown()
	{
		final AttributeDescriptor codeAttr = TypeManager.getInstance().getComposedType(Unit.class)
		                                                .getAttributeDescriptor(Unit.CODE);
		codeAttr.setUnique(unitCodeIsUniqueBefore);

		final AttributeDescriptor titleCodeAttr = TypeManager.getInstance().getComposedType(Title.class)
		                                                     .getAttributeDescriptor(Title.CODE);
		titleCodeAttr.setSearchable(titleCodeIsSearchableBefore);

		final AttributeDescriptor typeAttr = TypeManager.getInstance().getComposedType(Unit.class)
		                                                .getAttributeDescriptor(Unit.UNITTYPE);
		typeAttr.setUnique(unitTypeIsUniqueBefore);
	}

	@Test
	public void testInterceptorInstalled()
	{
		final InterceptorRegistry reg = ((DefaultModelService) modelService).getInterceptorRegistry();

		final Collection<ValidateInterceptor> validators = reg.getValidateInterceptors("Unit");

		assertThat(validators).isNotEmpty();
		boolean found = false;

		for (final ValidateInterceptor inter : validators)
		{
			if (inter instanceof UniqueAttributesInterceptor)
			{
				found = true;
				break;
			}
		}

		assertThat(found).isTrue();
	}

	@Test
	public void testUniqueChecks() throws ConsistencyCheckException
	{
		final UnitModel u1 = new UnitModel();
		u1.setCode("u1");
		u1.setUnitType("type");

		final UnitModel u2 = new UnitModel();
		u2.setCode("u2");
		u2.setUnitType("type");

		final User currentUser = jaloSession.getUser();
		try
		{

			modelService.saveAll(Arrays.asList(u1, u2));

			// test check via query
			try
			{
				final UnitModel um = new UnitModel();
				um.setCode("u1");
				um.setUnitType("type");

				modelService.save(um);

				failBecauseExceptionWasNotThrown(ModelSavingException.class);
			}
			catch (final ModelSavingException e)
			{
				final Throwable cause = getRootCause(e);
				assertThat(cause).isNotNull().isExactlyInstanceOf(AmbiguousUniqueKeysException.class);
				assertThat(((AmbiguousUniqueKeysException) cause).getAttributesKeys()).containsExactlyInAnyOrder(
						UnitModel.UNITTYPE, UnitModel.CODE);
			}

			// test in-memory check
			try
			{
				final UnitModel um1 = new UnitModel();
				um1.setCode("u3");
				um1.setUnitType("type");

				final UnitModel um2 = new UnitModel();
				um2.setCode("u3");
				um2.setUnitType("type");

				modelService.saveAll(Arrays.asList(um1, um2));
				failBecauseExceptionWasNotThrown(ModelSavingException.class);
			}
			catch (final ModelSavingException e)
			{
				final Throwable cause = getRootCause(e);
				assertThat(cause).isNotNull().isExactlyInstanceOf(AmbiguousUniqueKeysException.class);
				assertThat(((AmbiguousUniqueKeysException) cause).getAttributesKeys()).containsExactlyInAnyOrder(
						UnitModel.UNITTYPE, UnitModel.CODE);
			}

			final User u = currentUser.isAdmin() ? UserManager.getInstance().createUser("foo") : currentUser;
			// test search restriction being disabled
			// -> we create a search restriction which MUST fail if restrictions are not disabled
			TypeManager.getInstance().createRestriction("foo", u, TypeManager.getInstance().getComposedType(Unit.class),
					"?session.xyz IS NOT NULL");

			TestUtils.disableFileAnalyzer("log error expected");
			// test restriction failing just to be sure
			assertThat(((Object) jaloSession.getSessionContext().getAttribute("xyz"))).isNull();
			try
			{
				FlexibleSearch.getInstance().search("SELECT {PK} FROM {Unit}", Unit.class);
				failBecauseExceptionWasNotThrown(FlexibleSearchException.class);
			}
			catch (final FlexibleSearchException e)
			{
				assertThat(e).hasMessageContaining("xyz");
			}
			TestUtils.enableFileAnalyzer();

			// ok
			final UnitModel um4 = new UnitModel();
			um4.setCode("u4");
			um4.setUnitType("type");

			modelService.save(um4);

			// fail via query
			try
			{
				final UnitModel um1 = new UnitModel();
				um1.setCode("u1");
				um1.setUnitType("type");

				modelService.save(um1);
				failBecauseExceptionWasNotThrown(ModelSavingException.class);
			}
			catch (final ModelSavingException e)
			{
				final Throwable cause = getRootCause(e);
				assertThat(cause).isNotNull().isExactlyInstanceOf(AmbiguousUniqueKeysException.class);
				assertThat(((AmbiguousUniqueKeysException) cause).getAttributesKeys()).containsExactlyInAnyOrder(
						UnitModel.UNITTYPE, UnitModel.CODE);
			}
		}
		finally
		{
			jaloSession.setUser(currentUser);

		}
	}

	@Test
	public void testUniqueCheckWithEnums()
	{
		modelService.save(OrderStatus.valueOf("1"));
	}

	@Test
	public void testUnpersistedModelSave()
	{
		// create new OrderModel
		final OrderModel om = new OrderModel();
		modelService.initDefaults(om);
		om.setCurrency(i18nService.getCurrentCurrency());
		om.setDate(new Date());
		om.setNet(Boolean.FALSE);
		om.setUser(userService.getCurrentUser());
		final CatalogModel cm = modelService.create(CatalogModel.class);
		cm.setId("catalogId");
		final CatalogVersionModel cvm = modelService.create(CatalogVersionModel.class);
		cvm.setCatalog(cm);
		cvm.setVersion("catalogVersion");
		final ProductModel pm = modelService.create(ProductModel.class);
		pm.setCatalogVersion(cvm);
		pm.setCode("code");
		modelService.attach(pm);

		final UnitModel um = modelService.create(UnitModel.class);
		um.setCode("code");
		um.setUnitType("unitType");

		final ArrayList<AbstractOrderEntryModel> list = new ArrayList<>();

		//create new OrderEntryModel with order field set to unpersisted OrderModel
		final OrderEntryModel oem = new OrderEntryModel();
		modelService.initDefaults(oem);
		oem.setProduct(pm);
		oem.setQuantity(1L);
		oem.setUnit(um);

		oem.setOrder(om);

		list.add(oem);
		om.setEntries(list);

		//save OrderModel
		modelService.save(om);
	}


	@Test
	public void testAmbiguousUniqueItemInGermanMessage()
	{

		final UnitModel um1 = new UnitModel();
		um1.setCode("u3");
		um1.setUnitType("type");

		modelService.saveAll(List.of(um1));

		sessionService.executeInLocalView(new SessionExecutionBody()
		{
			@Override
			public void executeWithoutResult()
			{

				i18nService.setCurrentLocale(Locale.GERMAN);

				final UnitModel um2 = new UnitModel();
				um2.setCode("u3");
				um2.setUnitType("type");

				final Throwable throwable = catchThrowable(() -> modelService.saveAll(um1, um2));

				assertThat(throwable).isInstanceOf(ModelSavingException.class)
				                     .hasRootCauseInstanceOf(AmbiguousUniqueKeysException.class)
				                     .hasMessageEndingWith(UNIQUE_MANY_MSG_DE);

				final AmbiguousUniqueKeysException rootCause = (AmbiguousUniqueKeysException) getRootCause(throwable);
				assertThat(rootCause.getAttributesKeys()).isNotNull()
				                                         .containsExactlyInAnyOrder(UnitModel.CODE, UnitModel.UNITTYPE);
			}
		});
	}


	@Test
	public void testAmbiguousUniqueItemInEnglishMessage()
	{

		final UnitModel um1 = new UnitModel();
		um1.setCode("u3");
		um1.setUnitType("type");

		modelService.saveAll(List.of(um1));

		sessionService.executeInLocalView(new SessionExecutionBody()
		{
			@Override
			public void executeWithoutResult()
			{

				i18nService.setCurrentLocale(Locale.ENGLISH);

				final UnitModel um2 = new UnitModel();
				um2.setCode("u3");
				um2.setUnitType("type");

				final Throwable throwable = catchThrowable(() -> modelService.saveAll(um1, um2));

				assertThat(throwable).isInstanceOf(ModelSavingException.class)
				                     .hasRootCauseInstanceOf(AmbiguousUniqueKeysException.class)
				                     .hasMessageEndingWith(UNIQUE_MANY_MSG_EN);

				final AmbiguousUniqueKeysException rootCause = (AmbiguousUniqueKeysException) getRootCause(throwable);
				assertThat(rootCause.getAttributesKeys()).isNotNull()
				                                         .containsExactlyInAnyOrder(UnitModel.CODE, UnitModel.UNITTYPE);
			}
		});
	}


	@Test
	public void testAmbiguousUniqueItemInOneCtxGermanMessage()
	{

		sessionService.executeInLocalView(new SessionExecutionBody()
		{
			@Override
			public void executeWithoutResult()
			{

				i18nService.setCurrentLocale(Locale.GERMAN);

				final UnitModel um1 = new UnitModel();
				um1.setCode("u3");
				um1.setUnitType("type");

				final UnitModel um2 = new UnitModel();
				um2.setCode("u3");
				um2.setUnitType("type");

				final Throwable throwable = catchThrowable(() -> modelService.saveAll(um1, um2));

				assertThat(throwable).isInstanceOf(ModelSavingException.class)
				                     .hasRootCauseInstanceOf(AmbiguousUniqueKeysException.class)
				                     .hasMessageEndingWith(UNIQUE_AT_LEASTONE_MSG_DE);

				final AmbiguousUniqueKeysException rootCause = (AmbiguousUniqueKeysException) getRootCause(throwable);
				assertThat(rootCause.getAttributesKeys()).isNotNull()
				                                         .containsExactlyInAnyOrder(UnitModel.CODE, UnitModel.UNITTYPE);
			}
		});
	}

	@Test
	public void testAmbiguousUniqueItemInOneCtxEnglishMessage()
	{

		sessionService.executeInLocalView(new SessionExecutionBody()
		{
			@Override
			public void executeWithoutResult()
			{
				i18nService.setCurrentLocale(Locale.ENGLISH);

				final UnitModel um1 = new UnitModel();
				um1.setCode("u3");
				um1.setUnitType("type");

				final UnitModel um2 = new UnitModel();
				um2.setCode("u3");
				um2.setUnitType("type");

				final Throwable throwable = catchThrowable(() -> modelService.saveAll(um1, um2));

				assertThat(throwable).isInstanceOf(ModelSavingException.class)
				                     .hasRootCauseInstanceOf(AmbiguousUniqueKeysException.class)
				                     .hasMessageEndingWith(UNIQUE_AT_LEASTONE_MSG_EN);

				final AmbiguousUniqueKeysException rootCause = (AmbiguousUniqueKeysException) getRootCause(throwable);
				assertThat(rootCause.getAttributesKeys()).isNotNull()
				                                         .containsExactlyInAnyOrder(UnitModel.CODE, UnitModel.UNITTYPE);
			}
		});
	}


	@Test
	public void testAmbiguousUniqueItemInOneCtxForNonSearchableGermanMessage()
	{

		sessionService.executeInLocalView(new SessionExecutionBody()
		{
			@Override
			public void executeWithoutResult()
			{
				i18nService.setCurrentLocale(Locale.GERMAN);

				final TitleModel um1 = new TitleModel();
				um1.setCode("u4");
				final TitleModel um2 = new TitleModel();
				um1.setCode("u3");

				final Throwable throwable = catchThrowable(() -> modelService.saveAll(um1, um2));

				assertThat(throwable).isInstanceOf(ModelSavingException.class)
				                     .hasRootCauseInstanceOf(NonSearchableAttributeException.class)
				                     .hasMessageEndingWith(UNIQUE_NONSEARCHABLE__MSG_DE);

				final NonSearchableAttributeException rootCause = (NonSearchableAttributeException) getRootCause(throwable);
				assertThat(rootCause.getAttributesKeys()).isNotNull()
				                                         .containsExactlyInAnyOrder(UnitModel.CODE);
			}
		});
	}

	@Test
	public void testAmbiguousUniqueItemInOneCtxForNonSearchableEnglishMessage()
	{

		sessionService.executeInLocalView(new SessionExecutionBody()
		{
			@Override
			public void executeWithoutResult()
			{
				i18nService.setCurrentLocale(Locale.ENGLISH);

				final TitleModel um1 = new TitleModel();
				um1.setCode("u4");

				final TitleModel um2 = new TitleModel();
				um1.setCode("u3");

				final Throwable throwable = catchThrowable(() -> modelService.saveAll(um1, um2));

				assertThat(throwable).isInstanceOf(ModelSavingException.class)
				                     .hasRootCauseInstanceOf(NonSearchableAttributeException.class)
				                     .hasMessageEndingWith(UNIQUE_NONSEARCHABLE__MSG_EN);

				final NonSearchableAttributeException rootCause = (NonSearchableAttributeException) getRootCause(throwable);
				assertThat(rootCause.getAttributesKeys()).isNotNull()
				                                         .containsExactlyInAnyOrder(UnitModel.CODE);
			}
		});
	}

}
