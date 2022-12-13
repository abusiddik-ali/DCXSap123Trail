package de.hybris.platform.cronjob.jalo;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.cronjob.model.FlexibleSearchCronJobModel;
import de.hybris.platform.cronjob.model.MoveMediaJobModel;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.cronjob.CronJobService;
import de.hybris.platform.servicelayer.model.ModelService;

import javax.annotation.Resource;

import org.apache.commons.lang3.RandomStringUtils;
import org.junit.Test;

@IntegrationTest
public class FlexibleSearchCronJobTest extends ServicelayerBaseTest
{
	@Resource
	ModelService modelService;

	@Resource
	CronJobService cronJobService;

	@Test
	public void testIfFlexibleSearchCronJobEndsProperlyWhenGivenQueryLongerThan255Characters()
	{
		final MoveMediaJobModel job = createTestJob();
		final FlexibleSearchCronJobModel flexibleSearchCronJob = createTestFlexibleSearchCronJobWithQueryLongerThan255Characters(
				job);

		modelService.save(flexibleSearchCronJob);
		cronJobService.performCronJob(flexibleSearchCronJob, true);

		assertThat(cronJobService.isFinished(flexibleSearchCronJob)).isTrue();
	}

	private FlexibleSearchCronJobModel createTestFlexibleSearchCronJobWithQueryLongerThan255Characters(
			final MoveMediaJobModel testJob)
	{
		final FlexibleSearchCronJobModel flexibleSearchCronJob = modelService.create(FlexibleSearchCronJobModel.class);
		final int gimmickQueryStringLength = 125;
		final String randomizeLongString = RandomStringUtils.randomAlphabetic(gimmickQueryStringLength);

		flexibleSearchCronJob.setJob(testJob);
		flexibleSearchCronJob.setQuery(
				"select {pk} from {userGroup} where 1=2 and '" + randomizeLongString + "'='" + randomizeLongString + "'"
		);
		flexibleSearchCronJob.setFailOnUnknown(false);
		flexibleSearchCronJob.setDontNeedTotal(false);

		return flexibleSearchCronJob;
	}

	private MoveMediaJobModel createTestJob()
	{
		final MoveMediaJobModel job = new MoveMediaJobModel();
		job.setCode(RandomStringUtils.randomAlphabetic(5));
		modelService.save(job);
		modelService.detachAll();
		return job;
	}
}
