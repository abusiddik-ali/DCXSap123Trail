/*
 * Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.testframework;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.testframework.model.HybrisTestsResult;
import de.hybris.platform.testframework.model.SingleTestResultData;
import de.hybris.platform.testframework.model.TestSuiteData;

import java.util.Collection;
import java.util.Map;

import org.apache.log4j.Logger;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.JUnitCore;

import com.google.common.collect.Iterables;

@UnitTest
public class HybrisTestRunnerUnitTest
{
	private static final Logger LOG = Logger.getLogger(HybrisTestRunnerUnitTest.class);
	private final HybrisTestRunner runner = new HybrisTestRunner(new JUnitCore());

	@Before
	public void before()
	{
		runner.afterCreation();
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSingleNullTestClass() throws Exception
	{
		runner.runTest(null, "testOne(de.hybris.platform.testframework.SampleTest)");
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSingleNullTestMethod() throws Exception
	{
		runner.runTest(SampleTest.class, null);
	}

	@Test
	public void testSingleTestCaseSuccess() throws Exception
	{
		final HybrisTestsResult allResults = runner.runTest(SampleTest.class,
				"testOne(de.hybris.platform.testframework.SampleTest)");

		Assert.assertNotNull(allResults);
		//
		Assert.assertNotNull(allResults.getTestResults());
		final Map<Class, TestSuiteData> resultAsMap = allResults.getTestResults();
		Assert.assertNotNull(resultAsMap);
		Assert.assertEquals(1, resultAsMap.keySet().size());
		Assert.assertNotNull(Iterables.getOnlyElement(resultAsMap.values()));

		final Collection<SingleTestResultData> testCasesList = resultAsMap.get(SampleTest.class).getResults();
		Assert.assertEquals(1, testCasesList.size());

		Assert.assertNotNull(Iterables.getOnlyElement(testCasesList));
		final SingleTestResultData singleResult = Iterables.getOnlyElement(testCasesList);

		Assert.assertNull(singleResult.getError());
		Assert.assertNull(singleResult.getFailure());
		Assert.assertFalse(singleResult.isIgnored());
	}

	@Test
	public void testSingleTestFourAssumeFails() throws Exception
	{
		final HybrisTestsResult allResults = runner.runTest(SampleTest.class,
				"testFourAssumeFails(de.hybris.platform.testframework.SampleTest)");

		Assert.assertNotNull(allResults);

		Assert.assertNotNull(allResults.getTestResults());
		final Map<Class, TestSuiteData> resultAsMap = allResults.getTestResults();
		Assert.assertNotNull(resultAsMap);
		Assert.assertEquals(1, resultAsMap.keySet().size());
		Assert.assertNotNull(Iterables.getOnlyElement(resultAsMap.values()));

		final Collection<SingleTestResultData> testCasesList = resultAsMap.get(SampleTest.class).getResults();
		Assert.assertEquals(1, testCasesList.size());

		Assert.assertNotNull(Iterables.getOnlyElement(testCasesList));
		final SingleTestResultData singleResult = Iterables.getOnlyElement(testCasesList);

		Assert.assertNotNull(singleResult.getError());
		Assert.assertNotNull(singleResult.getFailure());
		Assert.assertTrue("Test case should be ignored since  assumption failed ", singleResult.isIgnored());
	}

	@Test
	public void testSingleTestFiveThrowExpectedException() throws Exception
	{
		final HybrisTestsResult allResults = runner.runTest(SampleTest.class,
				"testFiveThrowExpectedException(de.hybris.platform.testframework.SampleTest)");

		Assert.assertNotNull(allResults);

		Assert.assertNotNull(allResults.getTestResults());
		final Map<Class, TestSuiteData> resultAsMap = allResults.getTestResults();
		Assert.assertNotNull(resultAsMap);
		Assert.assertEquals(1, resultAsMap.keySet().size());
		Assert.assertNotNull(Iterables.getOnlyElement(resultAsMap.values()));

		final Collection<SingleTestResultData> testCasesList = resultAsMap.get(SampleTest.class).getResults();
		Assert.assertEquals(1, testCasesList.size());

		Assert.assertNotNull(Iterables.getOnlyElement(testCasesList));
		final SingleTestResultData singleResult = Iterables.getOnlyElement(testCasesList);

		Assert.assertNull(singleResult.getError());
		Assert.assertNull(singleResult.getFailure());
		Assert.assertFalse(singleResult.isIgnored());
	}

	@Test
	public void testSingleNotExistingTestCase() throws Exception
	{
		final HybrisTestsResult allResults = runner.runTest(SampleTest.class,
				"notExisting(de.hybris.platform.testframework.SampleTest)");

		Assert.assertNotNull(allResults);

		Assert.assertNotNull(allResults.getTestResults());
		final Map<Class, TestSuiteData> resultAsMap = allResults.getTestResults();
		Assert.assertNotNull(resultAsMap);
		Assert.assertEquals(1, resultAsMap.keySet().size());
		Assert.assertNotNull(Iterables.getOnlyElement(resultAsMap.values()));

		final TestSuiteData testSuiteData = Iterables.getOnlyElement(resultAsMap.values());
		Assert.assertNotNull(testSuiteData);

		Assert.assertNotNull(testSuiteData.getResults());
		final SingleTestResultData singleResult = Iterables.getOnlyElement(testSuiteData.getResults());

		Assert.assertNotNull(singleResult.getError());
		Assert.assertNotNull(singleResult.getFailure());
		Assert.assertFalse(singleResult.isIgnored());
	}

	@Test
	public void testSingleTestClassPositiveScenario() throws Exception
	{
		final HybrisTestsResult allResults = runner.runTest(SampleTest.class);

		Assert.assertNotNull(allResults);

		Assert.assertNotNull(allResults.getTestResults());
		final Map<Class, TestSuiteData> resultAsMap = allResults.getTestResults();
		Assert.assertNotNull(resultAsMap);
		Assert.assertEquals(1, resultAsMap.keySet().size());
		Assert.assertNotNull(Iterables.getOnlyElement(resultAsMap.values()));

		final Collection<SingleTestResultData> testCasesList = resultAsMap.get(SampleTest.class).getResults();
		Assert.assertEquals(4, testCasesList.size());
	}

	@Test
	public void testSingleTestClassNegativeScenarioFailingBefore() throws Exception
	{
		final HybrisTestsResult allResults = runner.runTest(SampleSuccessTest.class);

		Assert.assertNotNull(allResults);

		Assert.assertNotNull(allResults.getTestResults());
		final Map<Class, TestSuiteData> resultAsMap = allResults.getTestResults();
		Assert.assertNotNull(resultAsMap);
		Assert.assertEquals(1, resultAsMap.keySet().size());
		Assert.assertNotNull(Iterables.getOnlyElement(resultAsMap.values()));

		final Collection<SingleTestResultData> testCasesList = resultAsMap.get(SampleSuccessTest.class).getResults();
		Assert.assertEquals(1, testCasesList.size());
	}

	@Test
	public void testSingleTestClassNegativeScenarioFailingBeforeAssume() throws Exception
	{
		final HybrisTestsResult allResults = runner.runTest(SampleTestCornerCaseBeforeAssume.class);

		Assert.assertNotNull(allResults);

		Assert.assertNotNull(allResults.getTestResults());
		final Map<Class, TestSuiteData> resultAsMap = allResults.getTestResults();
		Assert.assertNotNull(resultAsMap);
		Assert.assertEquals(1, resultAsMap.keySet().size());
		Assert.assertNotNull(Iterables.getOnlyElement(resultAsMap.values()));

		final Collection<SingleTestResultData> testCasesList = resultAsMap.get(SampleTestCornerCaseBeforeAssume.class)
		                                                                  .getResults();
		Assert.assertEquals(1, testCasesList.size());
	}

	@Test
	public void testMultipleTestClassesFailingBefore() throws Exception
	{
		final HybrisTestsResult allResults = runner.runTest(SampleTest.class, SampleSuccessTest.class);

		Assert.assertNotNull(allResults);

		Assert.assertNotNull(allResults.getTestResults());
		final Map<Class, TestSuiteData> resultAsMap = allResults.getTestResults();
		Assert.assertNotNull(resultAsMap);
		Assert.assertEquals(2, resultAsMap.keySet().size());
		Assert.assertNotNull(resultAsMap.get(SampleTest.class));
		Assert.assertNotNull(resultAsMap.get(SampleSuccessTest.class));

		assertFailures(resultAsMap, SampleTest.class, 4);
		assertFailures(resultAsMap, SampleSuccessTest.class, 1);
	}

	private void assertFailures(final Map<Class, TestSuiteData> resultAsMap, final Class clazz, final int size)
	{
		final Collection<SingleTestResultData> testCasesList = resultAsMap.get(clazz).getResults();
		Assert.assertEquals(size, testCasesList.size());
	}


}
