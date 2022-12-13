/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.test;


import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.platform.comments.model.CommentModel;
import de.hybris.platform.comments.model.CommentTypeModel;
import de.hybris.platform.comments.model.ComponentModel;
import de.hybris.platform.comments.model.DomainModel;
import de.hybris.platform.core.model.user.UserGroupModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.testframework.seed.TestDataCreator;
import de.hybris.platform.util.Utilities;

import java.util.Collections;
import java.util.Date;
import java.util.UUID;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;


public abstract class RelationMarkModifiedPropertyIntegrationBaseTest extends ServicelayerBaseTest
{

	private static final int MILLIS = 10;
	private final PropertyConfigSwitcher persistenceLegacyMode = new PropertyConfigSwitcher("persistence.legacy.mode");
	private final PropertyConfigSwitcher navigableRelationMarkModified = new PropertyConfigSwitcher(
			"relation.PrincipalGroupRelation.markmodified");
	private final PropertyConfigSwitcher unnavigableRelationMarkModified = new PropertyConfigSwitcher(
			"relation.CommentItemRelation.markmodified");
	private final boolean persistenceLegacyModeValue;
	@Resource
	private ModelService modelService;
	private UserModel testUser;
	private UserGroupModel testGroup;
	private CommentModel testComment;

	public RelationMarkModifiedPropertyIntegrationBaseTest(final boolean persistenceLegacyModeValue)
	{
		this.persistenceLegacyModeValue = persistenceLegacyModeValue;
	}

	@Before
	public void setUp() throws Exception
	{
		persistenceLegacyMode.switchToValue(Boolean.toString(persistenceLegacyModeValue));

		Utilities.clearMarkModifiedOverrideCache();

		final TestDataCreator testDataCreator = new TestDataCreator(modelService);

		testUser = testDataCreator.createUser(UUID.randomUUID().toString(), "testUser");
		testGroup = testDataCreator.createUserGroup(UUID.randomUUID().toString(), "testGroup");
		testComment = createTestComment(testDataCreator);

	}

	private CommentModel createTestComment(final TestDataCreator testDataCreator)
	{
		final DomainModel domain = modelService.create(DomainModel.class);
		domain.setCode(UUID.randomUUID().toString());

		final ComponentModel component = modelService.create(ComponentModel.class);
		component.setCode(UUID.randomUUID().toString());
		component.setDomain(domain);

		final CommentTypeModel commentType = modelService.create(CommentTypeModel.class);
		commentType.setCode(UUID.randomUUID().toString());
		commentType.setDomain(domain);

		final CommentModel testComment = modelService.create(CommentModel.class);
		testComment.setCode(UUID.randomUUID().toString());
		testComment.setAuthor(testDataCreator.createUser(UUID.randomUUID().toString(), "commentingUser"));
		testComment.setCommentType(commentType);
		testComment.setComponent(component);
		testComment.setText("lorem ipsum");
		modelService.saveAll(domain, testComment);
		return testComment;
	}

	@After
	public void tearDown() throws Exception
	{
		persistenceLegacyMode.switchBackToDefault();
		navigableRelationMarkModified.switchBackToDefault();
		persistenceLegacyMode.switchBackToDefault();

		Utilities.clearMarkModifiedOverrideCache();
	}

	@Test
	public void shouldUpdateTimestampWhenAddingLinkWhileRelationPropertyIsTrue() throws InterruptedException
	{
		final Date userDate1 = testUser.getModifiedtime();

		setNavigableRelationMarkModified(true);

		//when
		Thread.sleep(MILLIS);
		testGroup.setMembers(Collections.singleton(testUser));

		modelService.save(testGroup);
		modelService.refresh(testUser);
		final Date userDate2 = testUser.getModifiedtime();

		assertThat(userDate1).isBefore(userDate2);
	}

	private void setUnnavigableRelationMarkModified(final boolean b)
	{
		unnavigableRelationMarkModified.switchToValue(Boolean.toString(b));
		Utilities.clearMarkModifiedOverrideCache();
	}

	private void setNavigableRelationMarkModified(final boolean value)
	{
		navigableRelationMarkModified.switchToValue(Boolean.toString(value));
		Utilities.clearMarkModifiedOverrideCache();
	}


	@Test
	public void shouldNotUpdateTimestampWhenAddingLinkWhileRelationPropertyIsFalse() throws InterruptedException
	{
		final Date userDate1 = testUser.getModifiedtime();

		setNavigableRelationMarkModified(false);

		//when
		Thread.sleep(MILLIS);
		testGroup.setMembers(Collections.singleton(testUser));

		modelService.save(testGroup);
		modelService.refresh(testUser);

		final Date userDate2 = testUser.getModifiedtime();

		assertThat(userDate1).isEqualTo(userDate2);
	}


	@Test
	public void shouldUpdateTimestampWhenRemovingLinkWhileRelationPropertyIsTrue() throws InterruptedException
	{
		setNavigableRelationMarkModified(true);

		testUser.setGroups(Collections.singleton(testGroup));

		modelService.save(testUser);
		modelService.refresh(testUser);

		final Date userDate1 = testUser.getModifiedtime();

		//when
		Thread.sleep(MILLIS);
		testGroup.setMembers(Collections.emptySet());
		modelService.save(testGroup);

		//then
		modelService.refresh(testUser);
		final Date userDate2 = testUser.getModifiedtime();
		assertThat(userDate1).isBefore(userDate2);
	}


	@Test
	public void shouldNotUpdateTimestampWhenRemovingLinkWhileRelationPropertyIsFalse() throws InterruptedException
	{

		setNavigableRelationMarkModified(false);

		testUser.setGroups(Collections.singleton(testGroup));

		modelService.save(testUser);
		modelService.refresh(testUser);

		final Date userDate1 = testUser.getModifiedtime();

		//when
		Thread.sleep(MILLIS);
		testGroup.setMembers(Collections.emptySet());
		modelService.save(testUser);

		//then
		modelService.refresh(testUser);
		final Date userDate2 = testUser.getModifiedtime();
		assertThat(userDate1).isEqualTo(userDate2);
	}


	@Test
	public void shouldUpdateTimestampWhenRemovingTargetWhileRelationPropertyIsTrue() throws InterruptedException
	{
		setNavigableRelationMarkModified(true);

		testUser.setGroups(Collections.singleton(testGroup));

		modelService.save(testUser);
		modelService.refresh(testUser);

		final Date userDate1 = testUser.getModifiedtime();

		//when
		Thread.sleep(MILLIS);
		modelService.remove(testGroup);

		//then
		modelService.refresh(testUser);
		final Date userDate2 = testUser.getModifiedtime();
		assertThat(userDate1).isBefore(userDate2);
	}


	@Test
	public void shouldNotUpdateTimestampWhenRemovingTargetWhileRelationPropertyIsFalse() throws InterruptedException
	{

		setNavigableRelationMarkModified(false);

		testUser.setGroups(Collections.singleton(testGroup));

		modelService.save(testUser);
		modelService.refresh(testUser);

		final Date userDate1 = testUser.getModifiedtime();

		//when
		Thread.sleep(MILLIS);
		modelService.remove(testGroup);

		//then
		modelService.refresh(testUser);
		final Date userDate2 = testUser.getModifiedtime();
		assertThat(userDate1).isEqualTo(userDate2);
	}

	@Test
	public void shouldUpdateTimestampWhenRemovingTargetWhileUnnavigableRelationPropertyIsTrue() throws InterruptedException
	{
		setUnnavigableRelationMarkModified(true);

		testUser.setComments(Collections.singletonList(testComment));

		modelService.save(testUser);
		modelService.refresh(testUser);

		final Date userDate1 = testUser.getModifiedtime();

		//when
		Thread.sleep(MILLIS);
		modelService.remove(testComment);

		//then
		modelService.refresh(testUser);
		final Date userDate2 = testUser.getModifiedtime();
		assertThat(userDate1).isBefore(userDate2);
	}

	@Test
	public void shouldNotUpdateTimestampWhenRemovingTargetWhileUnnavigableRelationPropertyIsFalse() throws InterruptedException
	{
		setUnnavigableRelationMarkModified(false);

		testUser.setComments(Collections.singletonList(testComment));

		modelService.save(testUser);
		modelService.refresh(testUser);

		final Date userDate1 = testUser.getModifiedtime();

		//when
		Thread.sleep(MILLIS);
		modelService.remove(testComment);

		//then
		modelService.refresh(testUser);
		final Date userDate2 = testUser.getModifiedtime();
		assertThat(userDate1).isEqualTo(userDate2);
	}

}
