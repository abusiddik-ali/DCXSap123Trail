/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.cronjob.jalo;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.enums.JobLogLevel;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.cronjob.model.JobModel;
import de.hybris.platform.cronjob.model.LogFileModel;
import de.hybris.platform.jdbcwrapper.DBOutageSimulator;
import de.hybris.platform.jdbcwrapper.DBOutageSimulator.DBOutage;
import de.hybris.platform.jdbcwrapper.HybrisDataSource;
import de.hybris.platform.jdbcwrapper.JUnitJDBCConnectionPool;
import de.hybris.platform.scripting.model.ScriptModel;
import de.hybris.platform.scripting.util.TestScriptLogicHolder;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.cronjob.CronJobService;
import de.hybris.platform.servicelayer.internal.model.ScriptingJobModel;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.daos.TitleDao;
import de.hybris.platform.testframework.PropertyConfigSwitcher;
import de.hybris.platform.testframework.TestUtils;
import de.hybris.platform.util.StrandedItemsRegistry;
import de.hybris.platform.util.Utilities;

import java.io.InputStream;
import java.time.Duration;
import java.time.Instant;
import java.util.Collection;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.zip.ZipInputStream;

import javax.annotation.Resource;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@IntegrationTest
public class AbortingCronJobWithoutProperClosingIntegrationTest extends ServicelayerBaseTest
{
	private final PropertyConfigSwitcher cronjobLogtodbThreshold = new PropertyConfigSwitcher("cronjob.logtodb.threshold");
	@Resource
	private CronJobService cronJobService;
	@Resource
	private ModelService modelService;
	@Resource
	private TitleDao titleDao;
	private JUnitJDBCConnectionPool connectionPool;
	private DBOutageSimulator dbOutageSimulator;
	private TestScriptLogicHolder testScripts;
	@Resource
	private MediaService mediaService;

	private static void throwStackOverflowException()
	{
		throwStackOverflowException();
	}

	private static StrandedItemsRegistry strandedCronJobRegistry()
	{
		return CronJobManager.getInstance().getStrandedCronJobsRegistry();
	}

	@Before
	public void setUp() throws Exception
	{
		cronjobLogtodbThreshold.switchToValue("INFO");

		final HybrisDataSource dataSource = getDataSource();
		assertThat(dataSource.getConnectionPool()).isInstanceOf(JUnitJDBCConnectionPool.class);
		connectionPool = (JUnitJDBCConnectionPool) dataSource.getConnectionPool();
		dbOutageSimulator = new DBOutageSimulator(connectionPool);

		testScripts = new TestScriptLogicHolder();

		TestUtils.disableFileAnalyzer("testing cronjobs during db outage - there will be a lot of errors");
	}

	@After
	public void tearDown() throws Exception
	{
		TestUtils.enableFileAnalyzer();

		connectionPool.resetTestMode();
		cronjobLogtodbThreshold.switchBackToDefault();
		testScripts.clear();
	}

	@Test
	public void shouldFinishBasicCronJobWithGivenLogic() throws TimeoutException, InterruptedException
	{
		final String titleCode = "groovyTitle";
		final CronJobModel cronjob = createScriptedCronJob(titleCode);

		assertThat(titleDao.findTitleByCode(titleCode)).isNull();

		cronJobService.performCronJob(cronjob, false);

		waitForCronJobFinished(cronjob, Duration.ofSeconds(30));

		assertThat(cronJobService.isFinished(cronjob)).isTrue();
		assertThat(cronJobService.isSuccessful(cronjob)).isTrue();
		assertThat(titleDao.findTitleByCode(titleCode)).isNotNull();
	}

	@Test
	public void shouldAddCronJobToBeCheckedIfDBOutageOccursDuringExecution()
			throws TimeoutException, InterruptedException, ExecutionException
	{
		final String titleCode = "groovyTitle";
		//create cronjob that will run for at least 6s
		final CronJobModel cronjob = createScriptedCronJob(titleCode, Duration.ofSeconds(4));

		assertThat(titleDao.findTitleByCode(titleCode)).isNull();
		cronJobService.performCronJob(cronjob, false);

		//wait for cronjob to start
		waitForCronJobRunning(cronjob, Duration.ofSeconds(30));

		//after 2s start the db outage that will last 10s
		Thread.sleep(TimeUnit.SECONDS.toMillis(2));
		final DBOutage dbOutage = dbOutageSimulator.simulateDBOutage(Duration.ofSeconds(10));

		//the cronjob should fail the logic and will never finish
		assertThatThrownBy(() -> waitForCronJobFinished(cronjob, Duration.ofSeconds(30))).isNotNull();

		//wait for the db outage to finish
		dbOutage.waitTillEnd(30, TimeUnit.SECONDS);

		//get fresh cronjob data from db
		modelService.detach(cronjob);
		Utilities.invalidateCache(cronjob.getPk());
		final CronJobModel cronJobDb = modelService.get(cronjob.getPk());

		//the cronjob on db is marked as running
		assertThat(cronJobService.isFinished(cronJobDb)).isFalse();
		//the cronjob should be marked for checking
		assertThat(strandedCronJobRegistry().getStrandedItems()).contains(cronJobDb.getPk());
		assertThat(strandedCronJobRegistry().getStrandedItemContext(cronJobDb.getPk())).isNotEmpty().hasValueSatisfying(
				ctx -> assertThat(ctx).isInstanceOf(CronJobManager.StrandedCronJobContext.class));

		//check the logs
		assertThat(cronjob.getLogText()).doesNotContain("error level after logic log");
	}

	@Test
	public void shouldNotAddCronJobToBeCheckedIfDBOutageOccursDuringExecutionButNoCallToDB()
			throws TimeoutException, InterruptedException, ExecutionException
	{
		final String titleCode = "groovyTitle-" + UUID.randomUUID().toString();
		//create cronjob that will run for at least 8s
		final CronJobModel cronjob = createScriptedCronJob(titleCode, Duration.ofSeconds(8));

		assertThat(titleDao.findTitleByCode(titleCode)).isNull();
		cronJobService.performCronJob(cronjob, false);

		//wait for cronjob to start
		waitForCronJobRunning(cronjob, Duration.ofSeconds(30));

		//after 2s start the db outage that will last 10s
		Thread.sleep(TimeUnit.SECONDS.toMillis(2));
		final DBOutage dbOutage = dbOutageSimulator.simulateDBOutage(Duration.ofSeconds(3));

		waitForCronJobFinished(cronjob, Duration.ofSeconds(30));

		//wait for the db outage to finish
		dbOutage.waitTillEnd(30, TimeUnit.SECONDS);

		//get fresh cronjob data from db
		modelService.detach(cronjob);
		Utilities.invalidateCache(cronjob.getPk());
		final CronJobModel cronJobDb = modelService.get(cronjob.getPk());

		//the cronjob on db is marked as finished and successful
		assertThat(cronJobService.isFinished(cronJobDb)).isTrue();
		assertThat(cronJobService.isSuccessful(cronJobDb)).isTrue();
		assertThat(titleDao.findTitleByCode(titleCode)).extracting(TitleModel::getCode, TitleModel::getName)
		                                               .contains(titleCode, "testName-" + titleCode);

		//the cronjob should not be marked for checking
		assertThat(strandedCronJobRegistry().getStrandedItems()).doesNotContain(cronjob.getPk());
	}

	@Test
	public void shouldNotAddCronJobToBeCheckedIfScriptThrowsException()
			throws TimeoutException, InterruptedException
	{
		final String titleCode = "groovyTitle";
		//create cronjob that will run for at least 6s

		final CronJobModel cronjob = createScriptedCronJob(testScripts.scriptWithLogic(modelService, () -> {
			LoggerFactory.getLogger(this.getClass()).info("started the script");
			throw new IllegalStateException("something wen horribly wrong");
		}));

		assertThat(titleDao.findTitleByCode(titleCode)).isNull();
		cronJobService.performCronJob(cronjob, false);

		//wait for cronjob to start
		waitForCronJobRunning(cronjob, Duration.ofSeconds(30));

		//the cronjob should fail the logic and will never finish
		waitForCronJobFinished(cronjob, Duration.ofSeconds(30));

		//get fresh cronjob data from db
		modelService.detach(cronjob);
		Utilities.invalidateCache(cronjob.getPk());
		final CronJobModel cronJobDb = modelService.get(cronjob.getPk());

		assertThat(cronJobService.isFinished(cronJobDb)).isTrue();
		assertThat(cronJobService.isError(cronJobDb)).isTrue();
		assertThat(CronJobManager.getInstance().getStrandedCronJobsRegistry().getStrandedItems()).doesNotContain(
				cronJobDb.getPk());
	}

	@Test
	public void shouldAppendLogsDuringStrandedCronJobRecovery() throws TimeoutException, InterruptedException, ExecutionException
	{
		final String titleCode = "groovyTitle";
		//create cronjob that will run for at least 6s
		final CronJobModel cronjob = createScriptedCronJob(titleCode, Duration.ofSeconds(4));
		cronjob.setLogToFile(true);
		modelService.save(cronjob);

		assertThat(titleDao.findTitleByCode(titleCode)).isNull();
		cronJobService.performCronJob(cronjob, false);

		//wait for cronjob to start
		waitForCronJobRunning(cronjob, Duration.ofSeconds(30));

		//after 2s start the db outage that will last 10s
		Thread.sleep(TimeUnit.SECONDS.toMillis(2));
		final DBOutage dbOutage = dbOutageSimulator.simulateDBOutage(Duration.ofSeconds(10));

		//the cronjob should fail the logic and will never finish
		assertThatThrownBy(() -> waitForCronJobFinished(cronjob, Duration.ofSeconds(30))).isNotNull();

		//wait for the db outage to finish
		dbOutage.waitTillEnd(30, TimeUnit.SECONDS);

		//get fresh cronjob data from db
		modelService.detach(cronjob);
		Utilities.invalidateCache(cronjob.getPk());
		final CronJobModel cronJobDb = modelService.get(cronjob.getPk());

		//the cronjob on db is marked as running
		assertThat(cronJobService.isFinished(cronJobDb)).isFalse();
		//the cronjob should be marked for checking
		assertThat(strandedCronJobRegistry().getStrandedItems()).contains(cronJobDb.getPk());

		assertThat(strandedCronJobRegistry().getStrandedItems()).contains(cronJobDb.getPk());
		strandedCronJobRegistry().checkStrandedItems(0);
		assertThat(strandedCronJobRegistry().getStrandedItems()).doesNotContain(cronJobDb.getPk());


		modelService.refresh(cronJobDb);
		assertThat(cronJobDb.getStatus()).isEqualTo(CronJobStatus.ABORTED);
		assertThat(cronJobDb.getRunningOnClusterNode()).isEqualTo(-1);
		final Collection<LogFileModel> logFiles = cronJobDb.getLogFiles();
		assertThat(logFiles).isNotEmpty();
		final String joinedLog = getLogsFromFiles(logFiles);
		assertThat(joinedLog).contains("error level after logic log");
	}

	private String getLogsFromFiles(final Collection<LogFileModel> logFiles)
	{
		final StringBuilder sb = new StringBuilder();
		for (final LogFileModel l : logFiles)
		{

			try (final InputStream streamFromMedia = mediaService.getStreamFromMedia(l))
			{
				final ZipInputStream zipInputStream = new ZipInputStream(streamFromMedia);
				while (zipInputStream.getNextEntry() != null)
				{
					sb.append(new String(zipInputStream.readNBytes(2048)));
				}
				return sb.toString();
			}
			catch (final Exception ignored)
			{
			}
		}

		return sb.toString();
	}


	@Test
	public void shouldNotAddCronJobToBeCheckedIfScriptThrowsStackOverflowException() throws TimeoutException, InterruptedException
	{
		final String titleCode = "groovyTitle";
		//create cronjob that will run for at least 6s

		final CronJobModel cronjob = createScriptedCronJob(testScripts.scriptWithLogic(modelService, () -> {
			LoggerFactory.getLogger(this.getClass()).info("started the script");
			throwStackOverflowException();
		}));

		assertThat(titleDao.findTitleByCode(titleCode)).isNull();
		cronJobService.performCronJob(cronjob, false);

		//wait for cronjob to start
		waitForCronJobRunning(cronjob, Duration.ofSeconds(30));

		//the cronjob should fail the logic and will never finish
		waitForCronJobFinished(cronjob, Duration.ofSeconds(30));

		//get fresh cronjob data from db
		modelService.detach(cronjob);
		Utilities.invalidateCache(cronjob.getPk());
		final CronJobModel cronJobDb = modelService.get(cronjob.getPk());

		assertThat(cronJobService.isFinished(cronJobDb)).isTrue();
		assertThat(cronJobService.isError(cronJobDb)).isTrue();
		assertThat(strandedCronJobRegistry().getStrandedItems()).doesNotContain(
				cronJobDb.getPk());
	}

	private void waitForCronJobState(final CronJobModel cronJob, final Duration timeout,
	                                 final Predicate<CronJobModel> statePredicate, final boolean throwTimeoutException)
			throws InterruptedException, TimeoutException
	{

		final Instant start = Instant.now();
		while (!statePredicate.test(cronJob))
		{
			Thread.sleep(50);
			modelService.refresh(cronJob);
			if (timeout.minus(Duration.between(start, Instant.now())).isNegative())
			{
				if (throwTimeoutException)
				{
					throw new TimeoutException();
				}
				else
				{
					return;
				}
			}
		}
	}

	private void waitForCronJobRunning(final CronJobModel cronJob, final Duration timeout)
			throws InterruptedException, TimeoutException
	{
		waitForCronJobRunning(cronJob, timeout, true);
	}

	private void waitForCronJobRunning(final CronJobModel cronJob,
	                                   final Duration timeout,
	                                   final boolean throwTimeoutException)
			throws InterruptedException, TimeoutException
	{
		waitForCronJobState(cronJob, timeout, cj -> cronJobService.isRunning(cj), throwTimeoutException);
	}

	private void waitForCronJobFinished(final CronJobModel cronJob, final Duration timeout)
			throws InterruptedException, TimeoutException
	{
		waitForCronJobFinished(cronJob, timeout, true);
	}

	private void waitForCronJobFinished(final CronJobModel cronJob,
	                                    final Duration timeout,
	                                    final boolean throwTimeoutException)
			throws InterruptedException, TimeoutException
	{
		waitForCronJobState(cronJob, timeout, cj -> cronJobService.isFinished(cj), throwTimeoutException);
	}

	private HybrisDataSource getDataSource()
	{
		return Registry.getCurrentTenant().getDataSource();
	}

	private CronJobModel createScriptedCronJob(final String titleCode)
	{
		return createScriptedCronJob(titleCode, Duration.ofMillis(10));
	}

	private CronJobModel createScriptedCronJob(final String titleCode,
	                                           final Duration scriptMinDuration)
	{
		return createScriptedCronJob(prepareScript(titleCode, scriptMinDuration));
	}

	private CronJobModel createScriptedCronJob(final ScriptModel script)
	{
		final ScriptingJobModel scriptingJobModel = prepareScriptingJob(script.getCode());
		return prepareCronJob("testCronJob-" + UUID.randomUUID().toString(), scriptingJobModel, JobLogLevel.INFO);
	}

	private ScriptModel prepareScript(final String testGroovyTitle, final Duration scriptMinDuration)
	{
		return testScripts.scriptWithLogic(modelService, () -> {
			final Logger log = LoggerFactory.getLogger(this.getClass());
			final TitleModel title = modelService.create(TitleModel.class);
			title.setCode(testGroovyTitle);
			modelService.save(title);
			log.info("info level log");
			log.warn("warning level log");
			log.error("error level log");
			Thread.sleep(Math.max(10, scriptMinDuration.toMillis()));
			log.error("error level after logic log");
			title.setName("testName-" + testGroovyTitle);
			modelService.save(title);
		});
	}

	private ScriptingJobModel prepareScriptingJob(final String scriptCode)
	{
		final ScriptingJobModel scriptingJob = modelService.create(ScriptingJobModel.class);
		scriptingJob.setCode("myGroovyJob-" + UUID.randomUUID().toString());
		scriptingJob.setScriptURI("model://" + scriptCode);

		return scriptingJob;

	}

	private CronJobModel prepareCronJob(final String code, final JobModel job, final JobLogLevel dbLogLevel)
	{
		final CronJobModel cronjob = modelService.create(CronJobModel.class);
		cronjob.setCode(code);
		cronjob.setSingleExecutable(Boolean.TRUE);
		cronjob.setJob(job);
		cronjob.setLogToDatabase(Boolean.TRUE);
		cronjob.setLogLevelDatabase(dbLogLevel);
		cronjob.setNodeID(Registry.getClusterID());
		modelService.save(cronjob);
		return cronjob;
	}
}
