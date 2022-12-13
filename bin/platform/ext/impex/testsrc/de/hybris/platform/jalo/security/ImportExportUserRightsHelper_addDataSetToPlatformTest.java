/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jalo.security;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.core.model.user.UserGroupModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.jalo.Item;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.impex.ImpExResource;
import de.hybris.platform.servicelayer.impex.ImportConfig;
import de.hybris.platform.servicelayer.impex.ImportResult;
import de.hybris.platform.servicelayer.impex.ImportService;
import de.hybris.platform.servicelayer.impex.impl.StreamBasedImpExResource;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.user.UserService;
import org.junit.Assert;
import org.junit.Test;

import javax.annotation.Resource;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Iterator;
import java.util.List;

// please note that all these tests are parameterized by overriding this class
// any change to the tests will also change the parametrized execution
@IntegrationTest
public class ImportExportUserRightsHelper_addDataSetToPlatformTest extends ServicelayerBaseTest
{
    @Resource
    public ImportService importService;

    @Resource
    public UserService userService;

    @Resource
    public ModelService modelService;

    @Resource
    public FlexibleSearchService flexibleSearchService;

    @Test
    public void shouldNotImportRawPrincipal()
    {
        // given
        int countPrincipals = selectCountFrom("Principal");

        // when
        ImpexRow row = new ImpexRow("Principal", prefixed("T01_principal"));
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));

        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(countPrincipals, selectCountFrom("Principal"));
    }

    @Test
    public void shouldNotImportNonPrincipal()
    {
        // given
        int countUsers = selectCountFrom("Principal");
        int countTitles = selectCountFromTitle();

        // when
        ImpexRow row = new ImpexRow("Title", prefixed("T01_title"));
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(countUsers, selectCountFrom("Principal"));
        Assert.assertEquals(countTitles, selectCountFromTitle());
    }

    @Test
    public void shouldNotImportNonExistingType()
    {
        // given
        int countUsers = selectCountFrom("Principal");

        // when
        ImpexRow row = new ImpexRow("Foo", prefixed("uid_foo"));
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(countUsers, selectCountFrom("Principal"));
    }

    @Test
    public void shouldCreateValidUserGroup()
    {
        // given
        String groupname = prefixed("T02_creating");
        int countGroups = selectCountFrom("UserGroup");

        // when
        ImpexRow row = new ImpexRow("UserGroup", groupname);
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(countGroups + 1, selectCountFrom("UserGroup"));
        Assert.assertNotNull(userService.getUserGroupForUID(groupname));
    }

    @Test
    public void shouldNotUpdateUserGroupWithoutMembers()
    {
        // given
        String groupname = prefixed("T02_edit_group");
        ImpexRow row = new ImpexRow("UserGroup", groupname);
        importService.importData(getCompleteImpexConfig(row));
        long hjmp1 = getHjmptsAndDetach(userService.getUserGroupForUID(groupname));
        int countGroups = selectCountFrom("UserGroup");

        // when import an existing UserGroup
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));
        UserGroupModel group = userService.getUserGroupForUID(groupname);
        long hjmp2 = getHjmptsAndDetach(group);

        // then expected: UserGroup was not updated / modified
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(hjmp1, hjmp2);
        Assert.assertEquals(countGroups, selectCountFrom("UserGroup"));
        Assert.assertEquals(0, group.getAllGroups().size());
    }

    @Test
    public void shouldCreateAndModifyUserGroupSubtype()
    {
        // given
        String groupname = prefixed("T02_exist_groupsubtype01");
        int groupCount = selectCountFrom("UserGroup");
        ImpexRow row = new ImpexRow("TestUserGroup", groupname, "admingroup");

        // expect: usergroup was created
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        UserGroupModel group = userService.getUserGroupForUID(groupname);
        Assert.assertEquals(1, group.getAllGroups().size());
        Assert.assertEquals(groupCount + 1, selectCountFrom("UserGroup"));

        // expect: usergroup was modified
        // when
        groupCount += 1;
        long hjmp1 = getHjmptsAndDetach(group);
        importResult = importService.importData(getCompleteImpexConfig(row));
        // then
        group = userService.getUserGroupForUID(groupname);
        long hjmp2 = getHjmptsAndDetach(group);

        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(1, group.getAllGroups().size());
        Assert.assertEquals(groupCount, selectCountFrom("UserGroup"));
        Assert.assertTrue(hjmp1 < hjmp2);
    }

    @Test
    public void shouldNotUpdateUserGroupSubtypeAsBaseType()
    {
        // given
        String groupname = prefixed("T02_exist_groupsubtype02");
        ImpexRow preparationRow = new ImpexRow("TestUserGroup", groupname);
        importService.importData(getCompleteImpexConfig(preparationRow));
        long hjmp1 = getHjmptsAndDetach(userService.getUserGroupForUID(groupname));
        int countGroups = selectCountFrom("UserGroup");

        // when import the group as supertype
        ImpexRow testRow = new ImpexRow("UserGroup", groupname);
        ImportResult importResult = importService.importData(getCompleteImpexConfig(testRow));
        long hjmp2 = getHjmptsAndDetach(userService.getUserGroupForUID(groupname));

        // then expected: UserGroup was not updated
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(hjmp1, hjmp2);
        Assert.assertEquals(countGroups, selectCountFrom("UserGroup"));
    }

    @Test
    public void shouldNotUpdateUserGroupAsItsSubtype()
    {
        // given
        String groupname = prefixed("T02_exist_groupsubtype03");
        ImpexRow preparationRow = new ImpexRow("UserGroup", groupname);
        importService.importData(getCompleteImpexConfig(preparationRow));
        long hjmp1 = getHjmptsAndDetach(userService.getUserGroupForUID(groupname));
        int countGroups = selectCountFrom("UserGroup");

        // when import the group as subtype
        ImpexRow testRow = new ImpexRow("TestUserGroup", groupname);
        ImportResult importResult = importService.importData(getCompleteImpexConfig(testRow));
        long hjmp2 = getHjmptsAndDetach(userService.getUserGroupForUID(groupname));

        // then expected: UserGroup was not updated
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(hjmp1, hjmp2);
        Assert.assertEquals(countGroups, selectCountFrom("UserGroup"));
    }

    @Test
    public void shouldNotCreateUserGroupWhenUIDExists()
    {
        // given
        String name = prefixed("T02_exist_user");
        ImpexRow preparationRow = new ImpexRow("User", name);
        importService.importData(getCompleteImpexConfig(preparationRow));
        long hjmp1 = getHjmptsAndDetach(userService.getUserForUID(name));
        int countGroups1 = selectCountFrom("UserGroup");

        // when import the group as existing User
        ImpexRow testRow = new ImpexRow("UserGroup", name);
        ImportResult importResult = importService.importData(getCompleteImpexConfig(testRow));
        long hjmp2 = getHjmptsAndDetach(userService.getUserForUID(name));
        int countGroups2 = selectCountFrom("UserGroup");

        // then nothing should happen
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(hjmp1, hjmp2);
        Assert.assertEquals(countGroups1 + 1, countGroups2);        // this will fail if servicelayer is used
    }

    @Test
    public void shouldCreateUserGroupAndAssignToValidOtherGroup()
    {
        // when
        String groupname = prefixed("T04_exist_group");
        ImpexRow row = new ImpexRow("UserGroup", groupname, "admingroup");
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));
        UserGroupModel ug = userService.getUserGroupForUID(groupname);

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(1, ug.getGroups().size());
        Assert.assertEquals("admingroup", ug.getGroups().iterator().next().getUid());
    }

    @Test
    public void shouldNotAssignUserGroupToSelfWhileCreating()
    {
        // given
        String groupname = prefixed("T04_member_same_as_ug");
        int groupCount = selectCountFrom("UserGroup");

        // when
        ImpexRow row = new ImpexRow("UserGroup", groupname, groupname);
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));

        // then is created but not assigned
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(groupCount + 1, selectCountFrom("UserGroup"));
        Assert.assertEquals(0, userService.getUserGroupForUID(groupname).getGroups().size());
    }

    @Test
    public void shouldNotAssignUserGroupToSelfWhileModifying()
    {
        // given
        String groupname = prefixed("T04_same_as_ug_mod");
        ImpexRow prepareRow = new ImpexRow("UserGroup", groupname);
        importService.importData(getCompleteImpexConfig(prepareRow));
        long hjmp1 = getHjmptsAndDetach(userService.getUserGroupForUID(groupname));

        // when
        ImpexRow testRow = new ImpexRow("UserGroup", groupname, groupname);
        ImportResult importResult = importService.importData(getCompleteImpexConfig(testRow));
        UserGroupModel ug = userService.getUserGroupForUID(groupname);
        long hjmp2 = getHjmptsAndDetach(ug);

        // then expected: usergroup is not modified, nor assigned to group
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(hjmp1, hjmp2);
        Assert.assertEquals(0, ug.getGroups().size());
    }

    @Test
    public void shouldNotAssignUserGroupToMakeCycles()
    {
        // given
        ImpexRow[] prepareRows = {
                new ImpexRow("UserGroup", prefixed("T04_cycle1")),
                new ImpexRow("UserGroup", prefixed("T04_cycle2"), prefixed("T04_cycle1")),
                new ImpexRow("UserGroup", prefixed("T04_cycle3"), prefixed("T04_cycle2"))
        };
        importService.importData(getCompleteImpexConfig(prepareRows));
        long hjmp1 = getHjmptsAndDetach(userService.getUserGroupForUID(prefixed("T04_cycle1")));

        // when
        ImpexRow testRow = new ImpexRow("UserGroup", prefixed("T04_cycle1"), prefixed("T04_cycle3"));
        ImportResult importResult = importService.importData(getCompleteImpexConfig(testRow));
        UserGroupModel ug = userService.getUserGroupForUID(prefixed("T04_cycle1"));
        long hjmp2 = getHjmptsAndDetach(ug);

        // then expected: usergroup is not modified, nor assigned to group
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(hjmp1, hjmp2);
        Assert.assertEquals(0, ug.getGroups().size());
    }

    @Test
    public void shouldAssignUserGroupToItsSubgroupAndSupergroup()
    {
        // given
        String superName = prefixed("T04_alreadyexist_target_super");
        String subName = prefixed("T04_alreadyexist_target_sub");
        String midName = prefixed("T04_alreadyexist_target");
        String testName = prefixed("T04_alreadyexist_test");
        ImpexRow[] prepareRows = {
                new ImpexRow("UserGroup", superName),
                new ImpexRow("UserGroup", midName, superName),
                new ImpexRow("UserGroup", subName, midName),
                new ImpexRow("UserGroup", testName, midName)
        };
        importService.importData(getCompleteImpexConfig(prepareRows));
        long hjmp1 = getHjmptsAndDetach(userService.getUserGroupForUID(testName));
        ImportResult importResult;
        long hjmp2;
        UserGroupModel ug;

        // when assign to current
        ImpexRow testRow = new ImpexRow("UserGroup", testName, midName);
        importResult = importService.importData(getCompleteImpexConfig(testRow));
        ug = userService.getUserGroupForUID(testName);
        hjmp2 = getHjmptsAndDetach(ug);

        // then expected: usergroup is modified, but not assigned to group
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertTrue(hjmp1 < hjmp2);
        Assert.assertEquals(1, ug.getGroups().size());
        Assert.assertEquals(2, ug.getAllGroups().size());

        // when assign to subgroup and supergroup of current
        ImpexRow testRow2 = new ImpexRow("UserGroup", testName,
                subName, testName);
        importResult = importService.importData(getCompleteImpexConfig(testRow2));
        ug = userService.getUserGroupForUID(testName);
        hjmp2 = getHjmptsAndDetach(ug);

        // then expected: usergroup is modified and assigned to both groups
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertTrue(hjmp1 < hjmp2);
        Assert.assertEquals(2, ug.getGroups().size());
        Assert.assertEquals(3, ug.getAllGroups().size());
    }

    @Test
    public void shouldCreateUserButNotAssignToNonexistentGroup()
    {
        // given
        int userCount = selectCountFrom("User");

        // when
        ImpexRow row = new ImpexRow("User", prefixed("T04_nonexistent_member"), prefixed("T04_foo_foo"));
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));

        // then is created but not assigned
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(userCount + 1, selectCountFrom("User"));
        UserModel user = userService.getUserForUID(prefixed("T04_nonexistent_member"));
        Assert.assertEquals(0, user.getGroups().size());
    }

    @Test
    public void shouldNotAssignUserGroupToNongroupPrincipal()
    {
        // when
        String groupname = prefixed("T04_nongroup_member");
        ImpexRow row = new ImpexRow("UserGroup", groupname, "admin");
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));

        // then is created but not assigned
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        UserGroupModel group = userService.getUserGroupForUID(groupname);
        Assert.assertEquals(0, group.getGroups().size());
    }

    @Test
    public void shouldCreateUser()
    {
        // given
        String username = prefixed("T05_create_user");
        int countPrincipals = selectCountFrom("Principal");

        // when
        ImpexRow row = new ImpexRow("User", username);
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(countPrincipals + 1, selectCountFrom("Principal"));
        Assert.assertNotNull(userService.getUserForUID(username));
    }

    @Test
    public void shouldCreateUserSubtype()
    {
        // given
        String username = prefixed("T05_create_employee");
        int countUsers = selectCountFrom("User");

        // when
        ImpexRow row = new ImpexRow("Employee", username);
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(countUsers + 1, selectCountFrom("User"));
        Assert.assertNotNull(userService.getUserForUID(username));
    }

    @Test
    public void shouldModifyExistingUser()
    {
        // this behaviour is different to UserGroups:  User without group assignment is modified (hjmpts++),
        // while UserGroup is not
        // given
        String username = prefixed("T05_modify_user");
        ImpexRow row = new ImpexRow("User", username);
        importService.importData(getCompleteImpexConfig(row));
        long hjmp1 = getHjmptsAndDetach(userService.getUserForUID(username));
        int countUsers = selectCountFrom("User");

        // when import an existing UserGroup
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));
        UserModel user = userService.getUserForUID(username);
        long hjmp2 = getHjmptsAndDetach(user);

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertTrue(hjmp1 < hjmp2);
        Assert.assertEquals(countUsers, selectCountFrom("User"));
        Assert.assertEquals(0, user.getAllGroups().size());
    }

    @Test
    public void shouldNotUpdateNonuserPrincipal()
    {
        // given
        String name = prefixed("T05_another_principal");
        ImpexRow prepareRow = new ImpexRow("TestUserGroup", name);
        importService.importData(getCompleteImpexConfig(prepareRow));
        long hjmp1 = getHjmptsAndDetach(userService.getUserGroupForUID(name));
        int countPrincipals = selectCountFrom("Principal");

        // when import an existing UserGroup as User
        ImpexRow testRow = new ImpexRow("User", name);
        ImportResult importResult = importService.importData(getCompleteImpexConfig(testRow));
        long hjmp2 = getHjmptsAndDetach(userService.getUserGroupForUID(name));

        // then expected: do not update the group, do not create a user
        // or importResult.isError() if single-threaded import
        Assert.assertTrue(importResult.isFinished());
        if (importResult.isSuccessful())
        {
            Assert.assertEquals(hjmp1, hjmp2);
            Assert.assertEquals(countPrincipals, selectCountFrom("Principal"));
        }
    }

    @Test
    public void shouldAssignUserToValidUserGroupWhileCreating()
    {
        // when
        String username = prefixed("T06_valid_usergroup");
        ImpexRow row = new ImpexRow("User", username, "admingroup");
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));
        UserModel user = userService.getUserForUID(username);

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(1, user.getGroups().size());
        Assert.assertEquals("admingroup", user.getGroups().iterator().next().getUid());
    }

    @Test
    public void shouldAssignUserToValidUserGroupWhileUpdating()
    {
        // given
        String username = prefixed("T06_valid_usergroup_mod");
        ImpexRow prepareRow = new ImpexRow("User", username);
        importService.importData(getCompleteImpexConfig(prepareRow));

        // when
        ImpexRow row = new ImpexRow("User", username, "admingroup");
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));
        UserModel user = userService.getUserForUID(username);

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(1, user.getGroups().size());
        Assert.assertEquals("admingroup", user.getGroups().iterator().next().getUid());
    }

    @Test
    public void shouldAssignUserToValidUserGroupSubtype()
    {
        // given
        String username = prefixed("T06_user_subtype");
        String groupname = prefixed("T06_group_subtype");
        ImpexRow prepareRow = new ImpexRow("TestUserGroup", groupname);
        importService.importData(getCompleteImpexConfig(prepareRow));

        // when
        ImpexRow row = new ImpexRow("User", username, groupname);
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));
        UserModel user = userService.getUserForUID(username);

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(1, user.getGroups().size());
        Assert.assertEquals(groupname, user.getGroups().iterator().next().getUid());
    }

    @Test
    public void shouldModifyUserAndNotAssignToNonexistentGroup()
    {
        // given
        String username = prefixed("T06_groupdoesnotexist_mod");
        ImpexRow prepareRow = new ImpexRow("User", username);
        importService.importData(getCompleteImpexConfig(prepareRow));
        long hjmp1 = getHjmptsAndDetach(userService.getUserForUID(username));

        // when
        ImpexRow row = new ImpexRow("User", username, prefixed("doesnotexist2"));
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));
        UserModel user = userService.getUserForUID(username);
        long hjmp2 = getHjmptsAndDetach(user);

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertTrue(hjmp1 < hjmp2);
        Assert.assertEquals(0, user.getGroups().size());
    }

    @Test
    public void shouldCreateUserButNotAssignToNongroupPrincipal()
    {
        // given
        String name = prefixed("T06_memberof_user");
        int princCount = selectCountFrom("Principal");

        // when
        ImpexRow row = new ImpexRow("User", name, "admin");
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));

        // then is created but not assigned
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(princCount + 1, selectCountFrom("Principal"));
        userService.getUserForUID(name);   // assert no exception
    }

    @Test
    public void shouldModifyUserAndKeepGroups()
    {
        // given
        String username = prefixed("T06_already_in_group");
        ImpexRow prepareRow = new ImpexRow("User", username, "admingroup");
        importService.importData(getCompleteImpexConfig(prepareRow));
        long hjmp1 = getHjmptsAndDetach(userService.getUserForUID(username));

        // when
        ImpexRow row = new ImpexRow("User", username, "admingroup");
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));
        UserModel user = userService.getUserForUID(username);
        long hjmp2 = getHjmptsAndDetach(user);

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertTrue(hjmp1 < hjmp2);
        Assert.assertEquals(1, user.getGroups().size());
    }

    @Test
    public void shouldModifyUserAndNotRemoveGroups()
    {
        // given
        String username = prefixed("T06_noremove");
        ImpexRow prepareRow = new ImpexRow("User", username, "admingroup");
        importService.importData(getCompleteImpexConfig(prepareRow));
        long hjmp1 = getHjmptsAndDetach(userService.getUserForUID(username));

        // when
        ImpexRow row = new ImpexRow("User", username);
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));
        UserModel user = userService.getUserForUID(username);
        long hjmp2 = getHjmptsAndDetach(user);

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertTrue(hjmp1 < hjmp2);
        Assert.assertEquals(1, user.getGroups().size());
    }

    @Test
    public void shouldBelongToCurrentAndSuperGroups()
    {
        // given
        String superName = prefixed("T00_target_super");
        String midName = prefixed("T00_target");
        String subName = prefixed("T00_target_sub");
        String username = prefixed("T00_groups");
        ImpexRow[] prepareRows = {
                new ImpexRow("UserGroup", superName),
                new ImpexRow("UserGroup", midName, superName),
                new ImpexRow("UserGroup", subName, midName)
        };
        importService.importData(getCompleteImpexConfig(prepareRows));

        // when
        ImpexRow row = new ImpexRow("User", username, midName);
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));
        UserModel user = userService.getUserForUID(username);

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(1, user.getGroups().size());
        Assert.assertEquals(2, user.getAllGroups().size());

        Iterator<PrincipalGroupModel> groups = user.getAllGroups().iterator();
        List<String> groupNames = List.of(midName, superName);

        while (groups.hasNext())
        {
            Assert.assertTrue(groupNames.contains(groups.next().getUid()));
        }
    }

    @Test
    public void shouldAssignUserToBothSubgroupAndSuperGroup()
    {
        // given
        String superName = prefixed("T06_alreadyexist_target_super");
        String midName = prefixed("T06_alreadyexist_target");
        String subName = prefixed("T06_alreadyexist_target_sub");
        String username1 = prefixed("T06_alreadyexist_user1");
        String username2 = prefixed("T06_alreadyexist_user2");
        ImpexRow[] prepareRows = {
                new ImpexRow("UserGroup", superName),
                new ImpexRow("UserGroup", midName, superName),
                new ImpexRow("UserGroup", subName, midName),
                new ImpexRow("User", username1, midName),
                new ImpexRow("User", username2, midName)
        };
        importService.importData(getCompleteImpexConfig(prepareRows));
        ImportResult importResult;
        UserModel user;
        ImpexRow testRow;

        // when assign to super of current
        testRow = new ImpexRow("User", username1, superName);
        importResult = importService.importData(getCompleteImpexConfig(testRow));
        user = userService.getUserForUID(username1);

        // then is assigned
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(2, user.getGroups().size());
        Assert.assertEquals(2, user.getAllGroups().size());

        // when assign to sub of current
        testRow = new ImpexRow("User", username2, subName);
        importResult = importService.importData(getCompleteImpexConfig(testRow));
        user = userService.getUserForUID(username2);

        // then is assigned
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(2, user.getGroups().size());     // because: shouldBelongToCurrentAndSuperGroups()
        Assert.assertEquals(3, user.getAllGroups().size());     // because: shouldBelongToCurrentAndSuperGroups()

        // when assign to all in hierarchy
        testRow = new ImpexRow("User", prefixed("T06_full_group_structure"),
                midName, subName, superName);
        importResult = importService.importData(getCompleteImpexConfig(testRow));
        user = userService.getUserForUID(prefixed("T06_full_group_structure"));

        // then all are assigned
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(3, user.getGroups().size());
        Assert.assertEquals(3, user.getAllGroups().size());
    }

    @Test
    public void shouldUpdateUserAsEmployee()
    {
        // this is strange and works differently to groups - but that's how it works...

        // given
        String name = prefixed("T00_user_as_employee");
        ImpexRow prepareRow = new ImpexRow("User", name);
        importService.importData(getCompleteImpexConfig(prepareRow));
        int countUsers = selectCountFrom("User");
        long hjmp1 = getHjmptsAndDetach(userService.getUserForUID(name));

        // when
        ImpexRow row = new ImpexRow("Employee", name);
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));
        UserModel user = userService.getUserForUID(name);
        long hjmp2 = getHjmptsAndDetach(user);

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(countUsers, selectCountFrom("User"));
        Assert.assertTrue(hjmp1 < hjmp2);
    }


    @Test
    public void shouldUpdateEmployeeAsUser()
    {
        // given
        String username = prefixed("T00_employee_as_user");
        ImpexRow prepareRow = new ImpexRow("Employee", username);
        importService.importData(getCompleteImpexConfig(prepareRow));
        int countUsers = selectCountFrom("User");
        long hjmp1 = getHjmptsAndDetach(userService.getUserForUID(username));

        // when
        ImpexRow row = new ImpexRow("User", username);
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));
        UserModel user = userService.getUserForUID(username);
        long hjmp2 = getHjmptsAndDetach(user);

        // then
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(countUsers, selectCountFrom("User"));
        Assert.assertTrue(hjmp1 < hjmp2);
    }

    /* --------- helper methods --------- */

    protected String prefixed(String uid)
    {
        return "import_userrightshelper_base_" + uid;
    }

    protected long getHjmptsAndDetach(ItemModel model)
    {
        long h = modelService.<Item>toPersistenceLayer(model).getPersistenceVersion();
        modelService.detach(model);
        return h;
    }

    protected int selectCountFrom(String itemtype)
    {
        FlexibleSearchQuery query = new FlexibleSearchQuery("select {pk} from {" + itemtype + "} where {uid} like '" + prefixed("%") + "'");
        query.setDisableCaching(true);
        return flexibleSearchService.search(query).getCount();
    }

    protected int selectCountFromTitle()
    {
        FlexibleSearchQuery query = new FlexibleSearchQuery("select {pk} from {Title}");
        query.setDisableCaching(true);
        return flexibleSearchService.search(query).getCount();
    }

    protected ImportConfig getCompleteImpexConfig(ImpexRow... rows)
    {
        ImportConfig config = getStandardConfig();
        config.setScript(asResource(composeImpex(rows)));
        return config;
    }

    protected static ImportConfig getStandardConfig()
    {
        ImportConfig config = new ImportConfig();
        config.setSynchronous(true);
        config.setLegacyMode(false);
        config.setFailOnError(true);
        config.setDistributedImpexEnabled(false);
        // direct persistence
        config.setSldForData(false);
        return config;
    }

    protected static String composeImpex(ImpexRow... rows)
    {
        final String impexHeader =
                "$START_USERRIGHTS;;;;\n" +
                        "Type;UID;MemberOfGroups;Password;Target\n";
        final String impexFooter =
                "$END_USERRIGHTS;;;;;\n";
        StringBuilder sb = new StringBuilder(impexHeader);
        for (ImpexRow row : rows)
            sb.append(row.toString());
        sb.append(impexFooter);
        return sb.toString();
    }

    protected static ImpExResource asResource(String impexText)
    {
        InputStream impexStream = new ByteArrayInputStream(impexText.getBytes());
        return new StreamBasedImpExResource(impexStream, "UTF-8");
    }

    // POJO, which neatly handles column values and .toString()
    static class ImpexRow
    {
        String type;
        String uid;
        String[] memberOfGroups;
        String password = "";
        String target = "";

        ImpexRow(String type, String uid, String... memberOfGroups)
        {
            this.type = type;
            this.uid = uid;
            this.memberOfGroups = memberOfGroups;
        }

        @Override
        public String toString()
        {
            if (type.isBlank() || uid.isBlank())
                throw new RuntimeException("Type and UID are required for this impex");

            return String.join(";", type, uid, String.join(",", memberOfGroups), password, target, "\n");
        }
    }
}