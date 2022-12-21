/*
 * Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved.
 */
package concerttours.service.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import de.hybris.bootstrap.annotations.IntegrationTest;
import de.hybris.platform.core.Registry;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.ServicelayerTest;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.variants.model.VariantProductModel;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import javax.annotation.Resource;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.jdbc.core.JdbcTemplate;

import concerttours.model.BandModel;
import concerttours.service.BandService;


/**
 *
 */
@IntegrationTest
public class DefaultBandServiceIntegrationTest extends ServicelayerTest
{
	@Resource
	private BandService bandService;

	@Resource
	private ModelService modelService;

	/** Test Band **/
	private BandModel bandModel;

	/* Code of test band */
	private static final String BAND_CODE = "101-JAZ";

	/* Name of test band */
	private static final String BAND_NAME = "Tight Notes";

	/* History of test band */
	private static final String BAND_HISTORY = "New contemporary, 7-piece Jaz from Kondon, formed in 2015";

	/* Album sold by test band */
	private static final Long ALBUM_SOLD = Long.valueOf(10L);

	@Before
	public void setup()
	{
		try
		{
			Thread.sleep(TimeUnit.SECONDS.toMillis(1));
			new JdbcTemplate(Registry.getCurrentTenant().getDataSource()).execute("CHECKPOINT");
			Thread.sleep(TimeUnit.SECONDS.toMillis(1));

		}
		catch (final InterruptedException exc)
		{
			//
		}

		// This instance of a BandModel will be used by the tests
		bandModel = modelService.create(BandModel.class);
		bandModel.setCode(BAND_CODE);
		bandModel.setName(BAND_NAME);
		bandModel.setAlbumSales(ALBUM_SOLD);
		bandModel.setHistory(BAND_HISTORY);

	}

	@Test(expected = UnknownIdentifierException.class)
	public void testFailBehavior()
	{
		bandService.getBandForCode(BAND_CODE);
	}

	/**
	 * This test tests and demonstrates that the Service's getAllBand method calls the DAOs' getAllBand method and
	 * returns the data it receives from it.
	 */
	@Test
	public void testBandService()
	{
		List<BandModel> bandModels = bandService.getBands();
		final int size = bandModels.size();
		modelService.save(bandModels);
		bandModels = bandService.getBands();
		assertEquals(size + 1, bandModels.size());
		assertEquals("Unepected band found", bandModel, bandModels.get(bandModels.size() - 1));

		final BandModel persistedBandModel = bandService.getBandForCode(BAND_CODE);
		assertNotNull("No band found", persistedBandModel);
		assertEquals("Different band found", bandModel, persistedBandModel);

	}

	/**
	 * This test tests and demonstrates that the Service's getAllBand method calls the DAOs' getAllBand method and
	 * returns the data it receives from it.
	 */
	@Test
	public void testBandServiceTours() throws Exception
	{
		createCoreData();
		importCsv("/impex/concerttours-bands.impex", "utf-8");
		//importCsv("/impex/concerttours-yBandtour.impex", "utf-8");
		final BandModel band = bandService.getBandForCode("A001");
		assertNotNull("No band found", band);
		final Set<ProductModel> tours = band.getTours();
		assertNotNull("No band found", tours);
		Assert.assertEquals("not found one tour", 1, tours.size());
		final Object[] objects = new Object[5];
		final Collection<VariantProductModel> concerts = ((ProductModel) tours.toArray(objects)[0]).getVariants();
		assertNotNull("No tour found", tours);
		Assert.assertEquals("no found one tour", 6, concerts.size());

	}
}