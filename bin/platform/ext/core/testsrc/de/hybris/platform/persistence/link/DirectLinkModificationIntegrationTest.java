package de.hybris.platform.persistence.link;

import static de.hybris.platform.core.model.link.LinkModel.SOURCE;
import static de.hybris.platform.core.model.link.LinkModel.TARGET;
import static de.hybris.platform.core.model.security.PrincipalGroupModel._PRINCIPALGROUPRELATION;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.PK;
import de.hybris.platform.core.model.link.LinkModel;
import de.hybris.platform.core.model.security.PrincipalModel;
import de.hybris.platform.core.model.user.UserGroupModel;
import de.hybris.platform.jalo.Item;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.jalo.link.Link;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import javax.annotation.Resource;

import org.junit.Test;

@IntegrationTest
public class DirectLinkModificationIntegrationTest extends ServicelayerBaseTest
{
	@Resource
	ModelService modelService;

	@Resource
	FlexibleSearchService flexibleSearchService;

	@Test
	public void shouldModifyLinkTargetFromServiceLayer()
	{
		final PK linkPk = createTestLink();
		final PK newTargetPk = createUserGroup();

		final LinkModel link = modelService.get(linkPk);
		final UserGroupModel newTarget = modelService.get(newTargetPk);

		link.setTarget(newTarget);
		modelService.saveAll();

		assertThat(link.getTarget()).isSameAs(newTarget);
		modelService.detachAll();

		final PK currentTargetPk = modelService.<LinkModel>get(linkPk).getTarget().getPk();
		assertThat(currentTargetPk).isEqualTo(newTargetPk);
	}

	@Test
	public void shouldModifyLinkSourceFromServiceLayer()
	{
		final PK linkPk = createTestLink();
		final PK newSourcePk = createUserGroup();

		final LinkModel link = modelService.get(linkPk);
		final UserGroupModel newSource = modelService.get(newSourcePk);

		link.setSource(newSource);
		modelService.saveAll();

		assertThat(link.getSource()).isSameAs(newSource);
		modelService.detachAll();

		final PK currentSourcePk = modelService.<LinkModel>get(linkPk).getSource().getPk();
		assertThat(currentSourcePk).isEqualTo(newSourcePk);
	}

	@Test
	public void shouldModifyLinkTargetFromJALOLayer()
	{
		final PK linkPk = createTestLink();
		final PK newTargetPk = createUserGroup();

		final Link link = JaloSession.lookupItem(linkPk);
		final Item newTarget = JaloSession.lookupItem(newTargetPk);

		link.setTarget(newTarget);

		final Item currentTarget = link.getTarget();
		assertThat(currentTarget).isSameAs(newTarget);

		final PK currentTargetPk = JaloSession.<Link>lookupItem(linkPk).getTarget().getPK();
		assertThat(currentTargetPk).isEqualTo(newTargetPk);
	}

	@Test
	public void shouldModifyLinkSourceFromJALOLayer()
	{
		final PK linkPk = createTestLink();
		final PK newSourcePk = createUserGroup();

		final Link link = JaloSession.lookupItem(linkPk);
		final Item newSource = JaloSession.lookupItem(newSourcePk);

		link.setSource(newSource);

		assertThat(link.getSource()).isSameAs(newSource);

		final PK currentSourcePk = JaloSession.<Link>lookupItem(linkPk).getSource().getPK();
		assertThat(currentSourcePk).isEqualTo(newSourcePk);
	}

	@Test
	public void shouldNullifyLinkTargetFromJALOLayer()
	{
		final Link link = JaloSession.lookupItem(createTestLink());

		link.setTarget(null);

		assertThat(link.getTarget()).isNull();
	}

	@Test
	public void shouldNullifyLinkSourceFromJALOLayer()
	{
		final Link link = JaloSession.lookupItem(createTestLink());

		link.setSource(null);

		assertThat(link.getSource()).isNull();
	}

	private PK createTestLink()
	{
		final PK userGroup1Pk = createUserGroup();
		final PK userGroup2Pk = createUserGroup();

		modelService.<UserGroupModel>get(userGroup1Pk).setMembers(Set.of(modelService.<PrincipalModel>get(userGroup2Pk)));
		modelService.saveAll();

		final String query = "select {PK} from {" + _PRINCIPALGROUPRELATION + "} where {" + SOURCE + "}=?source and {" + TARGET + "}=?target";

		final List<Object> links = flexibleSearchService.search(query,
				Map.of("source", userGroup2Pk, "target", userGroup1Pk)).getResult();
		assertThat(links).isNotNull().isNotEmpty().hasSize(1);

		final Object link = links.get(0);
		assertThat(link).isNotNull().isInstanceOf(LinkModel.class);

		modelService.detachAll();
		return ((LinkModel) link).getPk();
	}

	private PK createUserGroup()
	{
		final UserGroupModel userGroup = modelService.create(UserGroupModel.class);

		userGroup.setUid("UG-" + UUID.randomUUID());
		modelService.saveAll();
		modelService.detachAll();

		return userGroup.getPk();
	}

}