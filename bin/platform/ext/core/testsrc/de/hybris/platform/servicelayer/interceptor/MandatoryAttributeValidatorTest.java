/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.servicelayer.interceptor;


import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.order.payment.DebitPaymentInfoModel;
import de.hybris.platform.core.model.test.TestItemType2Model;
import de.hybris.platform.core.model.test.TestItemType3Model;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.core.systemsetup.datacreator.internal.CoreDataCreator;
import de.hybris.platform.servicelayer.ServicelayerTransactionalBaseTest;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.servicelayer.interceptor.impl.MandatoryAttributesValidator.MissingMandatoryAttributesException;
import de.hybris.platform.servicelayer.model.AbstractItemModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionExecutionBody;
import de.hybris.platform.servicelayer.session.SessionService;

import java.util.Collections;
import java.util.Locale;

import javax.annotation.Resource;

import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Lists;


@IntegrationTest
public class MandatoryAttributeValidatorTest extends ServicelayerTransactionalBaseTest
{

	@Resource
	private I18NService i18nService;

	@Resource
	private ModelService modelService;

	@Resource
	private SessionService sessionService;

	@Resource
	private CoreDataCreator c2lDataCreator;

	@Before
	public void prepare()
	{
		c2lDataCreator.populateDatabase();
	}


	@Test
	public void testCreate1()
	{
		final UserModel user = modelService.create(UserModel.class);


		final Throwable throwable = catchThrowable(() -> modelService.save(user));

		assertThat(throwable).isInstanceOf(ModelSavingException.class)
		                     .hasCauseExactlyInstanceOf(MissingMandatoryAttributesException.class);


		final MissingMandatoryAttributesException cause = (MissingMandatoryAttributesException) throwable.getCause();

		assertThat(cause).isNotNull().hasMessageContaining("[uid]");
		assertThat(cause.getMissingAttributes()).contains(UserModel.UID);
	}

	@Test
	public void testCreate2()
	{
		final DebitPaymentInfoModel model = new DebitPaymentInfoModel();
		//those mandatory set to null
		model.setBank(null);
		model.setBankIDNumber(null);

		//those mandatory set to not null
		model.setCode("codenameEagle");
		model.setAccountNumber("100-111-2344");
		model.setBaOwner("owner");


		final Throwable throwable = catchThrowable(() -> modelService.save(model));

		assertThat(throwable).isNotNull()
		                     .isExactlyInstanceOf(ModelSavingException.class)
		                     .hasMessageContaining(DebitPaymentInfoModel.BANK)
		                     .hasMessageContaining(DebitPaymentInfoModel.BANKIDNUMBER)
		                     .hasMessageContaining(DebitPaymentInfoModel.USER);

		assertThat(throwable.getCause()).isNotNull()
		                                .isExactlyInstanceOf(MissingMandatoryAttributesException.class)
		                                .hasMessageContaining(DebitPaymentInfoModel.BANK)
		                                .hasMessageContaining(DebitPaymentInfoModel.BANKIDNUMBER)
		                                .hasMessageContaining(DebitPaymentInfoModel.USER);

		assertThat(((MissingMandatoryAttributesException) throwable.getCause()).getMissingAttributes())
				.containsExactlyInAnyOrder(DebitPaymentInfoModel.BANK,
						DebitPaymentInfoModel.BANKIDNUMBER,
						DebitPaymentInfoModel.USER);
	}

	@Test
	public void testCreate3()
	{
		final UserModel user = new UserModel();
		user.setUid("yyy");
		modelService.initDefaults(user);
		modelService.save(user);
	}


	@Test
	public void testMissingMandatoryAttributeGermanMessage()
	{
		getOrCreateLanguage(Locale.GERMAN.getLanguage());

		sessionService.executeInLocalView(new SessionExecutionBody()
		{
			@Override
			public void executeWithoutResult()
			{

				i18nService.setCurrentLocale(Locale.GERMAN);

				final TitleModel model = modelService.create(TitleModel.class);

				final Throwable throwable = catchThrowable(() -> modelService.save(model));

				assertThat(throwable).isNotNull().isInstanceOf(ModelSavingException.class);
				assertThat(throwable.getCause()).isExactlyInstanceOf(MissingMandatoryAttributesException.class)
				                                .hasMessageEndingWith(
						                                "fehlende Werte für [code] in Modell TitleModel (<unsaved>) zum Erstellen einer neuen Title");

				final MissingMandatoryAttributesException cause = (MissingMandatoryAttributesException) throwable.getCause();
				assertThat(cause.getMissingAttributes()).containsExactly(TitleModel.CODE);
				assertThat(cause.getModel()).isNotNull().isInstanceOf(TitleModel.class);

			}
		});
	}

	@Test
	public void testMissingMandatoryAttributeEnglishMessage()
	{

		sessionService.executeInLocalView(new SessionExecutionBody()
		{
			@Override
			public void executeWithoutResult()
			{

				i18nService.setCurrentLocale(Locale.ENGLISH);

				final TitleModel model = modelService.create(TitleModel.class);


				final Throwable throwable = catchThrowable(() -> modelService.save(model));

				assertThat(throwable).isNotNull().isInstanceOf(ModelSavingException.class);
				assertThat(throwable.getCause()).isExactlyInstanceOf(MissingMandatoryAttributesException.class)
				                                .hasMessageEndingWith(
						                                "missing values for [code] in model TitleModel (<unsaved>) to create a new Title");

				final MissingMandatoryAttributesException cause = (MissingMandatoryAttributesException) throwable.getCause();
				assertThat(cause.getMissingAttributes()).containsExactly(TitleModel.CODE);
				assertThat(cause.getModel()).isNotNull().isInstanceOf(TitleModel.class);
			}
		});
	}

	@Test
	public void shouldProperlySaveModelWhenMandatoryReferenceIsAlive() throws Exception
	{
		// given
		final TestItemType2Model itemTypeTwo = createAndSaveItemTypeTwo("foo");

		// when
		final TestItemType3Model itemType3 = modelService.create(TestItemType3Model.class);
		itemType3.setItemTypeTwo(itemTypeTwo);
		itemType3.setItemsTypeTwo(Collections.emptyList());
		modelService.save(itemType3);

		// then
		checkModelState(itemTypeTwo);
		checkModelState(itemType3);
		assertThat(itemType3.getItemTypeTwo()).isEqualTo(itemTypeTwo);
	}

	private void checkModelState(final AbstractItemModel model)
	{
		assertThat(modelService.isUpToDate(model)).isTrue();
		assertThat(modelService.isNew(model)).isFalse();
		assertThat(modelService.isRemoved(model)).isFalse();
	}

	@Test
	public void shouldThrowExceptionWhenMandatoryReferencedModelIsAlreadyRemoved() throws Exception
	{
		// given
		final TestItemType2Model itemTypeTwo = createAndSaveItemTypeTwo("foo");
		removeItemTypeTwo(itemTypeTwo);


		// when
		final TestItemType3Model itemType3 = modelService.create(TestItemType3Model.class);
		itemType3.setItemTypeTwo(itemTypeTwo);
		itemType3.setItemsTypeTwo(Collections.emptyList());

		final Throwable throwable = catchThrowable(() -> modelService.save(itemType3));

		assertThat(throwable).isNotNull().isInstanceOf(ModelSavingException.class);

		assertThat(throwable.getCause()).isExactlyInstanceOf(MissingMandatoryAttributesException.class)
		                                .hasMessageContaining("missing values for [itemTypeTwo]");

		final MissingMandatoryAttributesException cause = (MissingMandatoryAttributesException) throwable.getCause();
		assertThat(cause.getMissingAttributes()).containsExactlyInAnyOrder(TestItemType3Model.ITEMTYPETWO);
		assertThat(cause.getModel()).isNotNull().isInstanceOf(TestItemType3Model.class);
	}

	@Test
	public void shouldThrowExceptionWhenMandatoryModelAsPartOfReferencedCollectionIsAlreadyRemoved() throws Exception
	{
		// given
		final TestItemType2Model itemTypeTwo1 = createAndSaveItemTypeTwo("foo1");
		final TestItemType2Model itemTypeTwo2 = createAndSaveItemTypeTwo("foo2");
		final TestItemType2Model itemTypeTwo3 = createAndSaveItemTypeTwo("foo3");
		removeItemTypeTwo(itemTypeTwo2);
		removeItemTypeTwo(itemTypeTwo3);

		// when
		final TestItemType3Model itemType3 = modelService.create(TestItemType3Model.class);
		itemType3.setItemTypeTwo(itemTypeTwo1);
		itemType3.setItemsTypeTwo(Lists.newArrayList(itemTypeTwo2, itemTypeTwo3));

		final Throwable throwable = catchThrowable(() -> modelService.save(itemType3));

		assertThat(throwable).isNotNull().isInstanceOf(ModelSavingException.class);
		assertThat(throwable.getCause()).isExactlyInstanceOf(MissingMandatoryAttributesException.class)
		                                .hasMessageContaining("missing values for [itemsTypeTwo]");

		final MissingMandatoryAttributesException cause = (MissingMandatoryAttributesException) throwable.getCause();
		assertThat(cause.getMissingAttributes()).containsExactlyInAnyOrder(TestItemType3Model.ITEMSTYPETWO);
		assertThat(cause.getModel()).isNotNull().isInstanceOf(TestItemType3Model.class);
	}

	private TestItemType2Model createAndSaveItemTypeTwo(final String fooValue)
	{
		final TestItemType2Model itemType2 = modelService.create(TestItemType2Model.class);
		itemType2.setFoo(fooValue);
		modelService.save(itemType2);

		return itemType2;
	}

	private void removeItemTypeTwo(final TestItemType2Model itemType2)
	{
		modelService.remove(itemType2);
		assertThat(modelService.isRemoved(itemType2)).isTrue();
	}
}
