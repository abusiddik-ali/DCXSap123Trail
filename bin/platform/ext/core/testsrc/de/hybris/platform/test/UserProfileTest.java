package de.hybris.platform.test;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.hmc.jalo.UserProfile;
import de.hybris.platform.jalo.ConsistencyCheckException;
import de.hybris.platform.jalo.c2l.Language;
import de.hybris.platform.jalo.user.User;
import de.hybris.platform.jalo.user.UserGroup;
import de.hybris.platform.jalo.user.UserManager;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.internal.jalo.ServicelayerManager;

import java.util.Arrays;
import java.util.Set;

import org.junit.Test;

@IntegrationTest
public class UserProfileTest extends ServicelayerBaseTest
{

	ServicelayerManager servicelayerManager = ServicelayerManager.getInstance();
	private final String ISOCODE_EN = "en";
	private final String ISOCODE_DE = "de";

	@Test
	public void testUserGetsWriteableRightsToTheReadableLanguageWhenNoWriteableOneWasSpecified() throws ConsistencyCheckException
	{
		final User user = UserManager.getInstance().createEmployee("testUser1");
		final UserGroup userGroup = UserManager.getInstance().createUserGroup("userGroup1");
		final UserProfile userProfile = servicelayerManager.getOrCreateUserProfile();
		final Language language = getOrCreateLanguage(ISOCODE_EN);

		userGroup.setReadableLanguages(Set.of(language));
		user.setGroups(Set.of(userGroup));
		userProfile.setOwner(user);
		user.setUserprofile(userProfile);

		assertThat(Arrays.toString(user.getUserprofile().getAllReadableLanguages().toArray())).contains(ISOCODE_EN);
		assertThat(Arrays.toString(user.getUserprofile().getAllWritableLanguages().toArray())).contains(ISOCODE_EN);
		assertThat(user.getUserprofile().getAllReadableLanguages()).isEqualTo(user.getUserprofile().getAllWritableLanguages());
	}

	@Test
	public void testUserDoesntGetReadableLanguageAsWriteableWhenADifferentOneIsSpecified() throws ConsistencyCheckException
	{
		final User user = UserManager.getInstance().createEmployee("testUser2");
		final UserGroup userGroup = UserManager.getInstance().createUserGroup("userGroup2");
		final UserProfile userProfile = servicelayerManager.getOrCreateUserProfile();
		final Language languageEn = getOrCreateLanguage(ISOCODE_EN);
		final Language languageDe = getOrCreateLanguage(ISOCODE_DE);

		userGroup.setReadableLanguages(Set.of(languageEn));
		userGroup.setWriteableLanguages(Set.of(languageDe));
		user.setGroups(Set.of(userGroup));
		userProfile.setOwner(user);
		user.setUserprofile(userProfile);

		assertThat(Arrays.toString(user.getUserprofile().getAllReadableLanguages().toArray())).contains(ISOCODE_EN);
		assertThat(Arrays.toString(user.getUserprofile().getAllWritableLanguages().toArray())).contains(ISOCODE_DE);
		assertThat(user.getUserprofile().getAllReadableLanguages()).isNotEqualTo(user.getUserprofile().getAllWritableLanguages());
	}

	@Test
	public void testUserDoesntGetAnyWriteableWhenDenyWritePermissionForAllLanguagesSetToTrue() throws ConsistencyCheckException
	{
		final User user = UserManager.getInstance().createEmployee("testUser3");
		final UserGroup userGroup = UserManager.getInstance().createUserGroup("userGroup3");
		final UserProfile userProfile = servicelayerManager.getOrCreateUserProfile();
		final Language languageEn = getOrCreateLanguage(ISOCODE_EN);

		userGroup.setReadableLanguages(Set.of(languageEn));
		userGroup.setDenyWritePermissionForAllLanguages(true);
		user.setGroups(Set.of(userGroup));
		userProfile.setOwner(user);
		user.setUserprofile(userProfile);

		assertThat(Arrays.toString(user.getUserprofile().getAllReadableLanguages().toArray())).contains(ISOCODE_EN);
		assertThat(user.getUserprofile().getAllWritableLanguages().toArray()).isEmpty();
	}

	@Test
	public void testDenyWritePermissionInOneGroupDoesntInterfereWithLanguagesDefinedInOtherGroups()
			throws ConsistencyCheckException
	{
		final User user = UserManager.getInstance().createEmployee("testUser4");
		final UserGroup userGroup1 = UserManager.getInstance().createUserGroup("userGroup4");
		final UserGroup userGroup2 = UserManager.getInstance().createUserGroup("userGroup5");
		final UserProfile userProfile = servicelayerManager.getOrCreateUserProfile();
		final Language languageEn = getOrCreateLanguage(ISOCODE_EN);
		final Language languageDe = getOrCreateLanguage(ISOCODE_DE);

		userGroup1.setReadableLanguages(Set.of(languageEn));
		userGroup1.setDenyWritePermissionForAllLanguages(true);
		userGroup2.setWriteableLanguages(Set.of(languageDe));
		userGroup2.setReadableLanguages(Set.of(languageDe));
		user.setGroups(Set.of(userGroup1, userGroup2));
		userProfile.setOwner(user);
		user.setUserprofile(userProfile);

		assertThat(Arrays.toString(user.getUserprofile().getAllReadableLanguages().toArray())).contains(ISOCODE_EN, ISOCODE_DE);
		assertThat(Arrays.toString(user.getUserprofile().getAllWritableLanguages().toArray())).contains(ISOCODE_DE);
		assertThat(Arrays.toString(user.getUserprofile().getAllWritableLanguages().toArray())).doesNotContain(ISOCODE_EN);
	}

	@Test
	public void testUserGetsWritePermissionToAllDefinedLanguagesWhenOneGroupDeniesWriteToAllAndOtherDoesNotDefineAnyLanguage()
			throws ConsistencyCheckException
	{
		final User user = UserManager.getInstance().createEmployee("testUser4");
		final UserGroup userGroup1 = UserManager.getInstance().createUserGroup("userGroup4");
		final UserGroup userGroup2 = UserManager.getInstance().createUserGroup("userGroup5");
		final UserProfile userProfile = servicelayerManager.getOrCreateUserProfile();
		final Language languageEn = getOrCreateLanguage(ISOCODE_EN);
		final Language languageDe = getOrCreateLanguage(ISOCODE_DE);

		userGroup1.setReadableLanguages(Set.of(languageEn));
		userGroup1.setDenyWritePermissionForAllLanguages(true);
		userGroup2.setReadableLanguages(Set.of(languageDe));
		user.setGroups(Set.of(userGroup1, userGroup2));
		userProfile.setOwner(user);
		user.setUserprofile(userProfile);

		assertThat(Arrays.toString(user.getUserprofile().getAllReadableLanguages().toArray())).contains(ISOCODE_EN, ISOCODE_DE);
		assertThat(Arrays.toString(user.getUserprofile().getAllWritableLanguages().toArray())).contains(ISOCODE_EN, ISOCODE_DE);
	}

	@Test
	public void testUserHaveNoWritePermissionWhenMultipleGroupsDefineDenyWritePermissionForAllLanguages()
			throws ConsistencyCheckException
	{
		final User user = UserManager.getInstance().createEmployee("testUser5");
		final UserGroup userGroup1 = UserManager.getInstance().createUserGroup("userGroup6");
		final UserGroup userGroup2 = UserManager.getInstance().createUserGroup("userGroup7");
		final UserProfile userProfile = servicelayerManager.getOrCreateUserProfile();
		final Language languageEn = getOrCreateLanguage(ISOCODE_EN);
		final Language languageDe = getOrCreateLanguage(ISOCODE_DE);

		userGroup1.setReadableLanguages(Set.of(languageEn));
		userGroup1.setDenyWritePermissionForAllLanguages(true);
		userGroup2.setReadableLanguages(Set.of(languageDe));
		userGroup2.setDenyWritePermissionForAllLanguages(true);
		user.setGroups(Set.of(userGroup1, userGroup2));
		userProfile.setOwner(user);
		user.setUserprofile(userProfile);

		assertThat(Arrays.toString(user.getUserprofile().getAllReadableLanguages().toArray())).contains(ISOCODE_EN, ISOCODE_DE);
		assertThat(user.getUserprofile().getAllWritableLanguages().toArray()).isEmpty();
	}
}
