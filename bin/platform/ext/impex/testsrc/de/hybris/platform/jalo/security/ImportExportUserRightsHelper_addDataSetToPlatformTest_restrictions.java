/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.jalo.security;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.type.SearchRestrictionModel;
import de.hybris.platform.core.model.user.UserGroupModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.servicelayer.impex.ImportResult;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.type.TypeService;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import javax.annotation.Resource;

@IntegrationTest
public class ImportExportUserRightsHelper_addDataSetToPlatformTest_restrictions extends ImportExportUserRightsHelper_addDataSetToPlatformTest
{
    @Resource
    public TypeService typeService;

    private static final String RESTRICTION_CODE = "userGroupSearchRestriction";

    @Before
    public void setRestrictions()
    {
        ModelService modelService = Registry.getApplicationContext().getBean("modelService", ModelService.class);
        TypeService typeService = Registry.getApplicationContext().getBean("typeService", TypeService.class);

        SearchRestrictionModel searchRestrictionModel = modelService.create(SearchRestrictionModel.class);
        searchRestrictionModel.setQuery("{pk} IS NULL");
        searchRestrictionModel.setPrincipal(getCurrentUser());
        searchRestrictionModel.setActive(Boolean.TRUE);
        searchRestrictionModel.setGenerate(Boolean.FALSE);
        searchRestrictionModel.setCode(RESTRICTION_CODE);
        searchRestrictionModel.setRestrictedType(typeService.getComposedTypeForClass(UserGroupModel.class));
        modelService.save(searchRestrictionModel);
    }

    @After
    public void clearRestrictions()
    {
        ModelService modelService = Registry.getApplicationContext().getBean("modelService", ModelService.class);
        FlexibleSearchService fss = Registry.getApplicationContext().getBean("flexibleSearchService", FlexibleSearchService.class);
        SearchRestrictionModel restr = fss
                .<SearchRestrictionModel>search("select {pk} from {SearchRestriction} where {code}='" + RESTRICTION_CODE + "'")
                .getResult().get(0);
        modelService.remove(restr);
    }

    protected static UserModel getCurrentUser()
    {
        ModelService modelService = Registry.getApplicationContext().getBean("modelService", ModelService.class);
        return modelService.toModelLayer(JaloSession.getCurrentSession().getUser());
    }

    protected void toggleRestriction(boolean toggle)
    {
        SearchRestrictionModel restr = flexibleSearchService.
                <SearchRestrictionModel>search("select {pk} from {searchrestriction} where {code}='" + RESTRICTION_CODE + "'")
                .getResult()
                .get(0);
        restr.setActive(toggle);
        modelService.save(restr);
    }

    @Override
    protected String prefixed(String uid)
    {
        return "import_userrightshelper_restr_" + uid;
    }

    /* 2 tests to confirm restrictions working; all the other ignored tests would fail because of restrictions */

    @Test
    @Override
    public void shouldAssignUserToValidUserGroupWhileCreating()
    {
        String name = prefixed("T06_valid_usergroup");

        // when
        ImpexRow row = new ImpexRow("User", name, "admingroup");
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));

        // then
        toggleRestriction(false);
        UserModel user = userService.getUserForUID(name);
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(0, user.getGroups().size());  // restrictions did not allow assignment to group
    }

    @Test
    @Override
    public void shouldCreateValidUserGroup()
    {
        // given
        String name = prefixed("T02_newgroup");
        toggleRestriction(false);
        int countGroups = selectCountFrom("UserGroup");
        toggleRestriction(true);

        // when
        ImpexRow row = new ImpexRow("UserGroup", name, "admingroup");
        ImportResult importResult = importService.importData(getCompleteImpexConfig(row));

        // then
        toggleRestriction(false);
        Assert.assertTrue(importResult.isFinished());
        Assert.assertTrue(importResult.isSuccessful());
        Assert.assertEquals(countGroups + 1, selectCountFrom("UserGroup"));  // restriction allows creation
        Assert.assertEquals(0, userService.getUserGroupForUID(name).getGroups().size());  // but disallows read during creation

    }

    @Ignore
    @Test
    @Override
    public void shouldNotUpdateNonuserPrincipal()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldAssignUserToValidUserGroupSubtype()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldModifyUserAndNotRemoveGroups()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldNotUpdateUserGroupSubtypeAsBaseType()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldModifyUserAndKeepGroups()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldAssignUserToBothSubgroupAndSuperGroup()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldCreateUserGroupAndAssignToValidOtherGroup()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldNotCreateUserGroupWhenUIDExists()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldAssignUserToValidUserGroupWhileUpdating()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldBelongToCurrentAndSuperGroups()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldCreateAndModifyUserGroupSubtype()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldNotAssignUserGroupToSelfWhileModifying()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldNotAssignUserGroupToSelfWhileCreating()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldNotUpdateUserGroupAsItsSubtype()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldAssignUserGroupToItsSubgroupAndSupergroup()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldNotAssignUserGroupToNongroupPrincipal()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldNotAssignUserGroupToMakeCycles()
    {
    }

    @Ignore
    @Test
    @Override
    public void shouldNotUpdateUserGroupWithoutMembers()
    {
    }
}
