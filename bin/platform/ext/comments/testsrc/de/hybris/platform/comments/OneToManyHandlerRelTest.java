package de.hybris.platform.comments;

import static org.assertj.core.api.Assertions.assertThat;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.catalog.jalo.CatalogManager;
import de.hybris.platform.comments.jalo.CommentsManager;
import de.hybris.platform.testframework.HybrisJUnit4Test;
import de.hybris.platform.util.OneToManyHandler;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.function.Supplier;
import java.util.stream.Stream;

import org.junit.Test;
import org.springframework.util.ReflectionUtils;

@UnitTest
public class OneToManyHandlerRelTest extends HybrisJUnit4Test
{

	@Test
	public void shouldReturnCorrectQualifierForOneToManyRelations()
	{
		//given
		final Object[][] relationHandlersWithCorrectQualifiers = getRelationHandlersWithCorrectQualifiers();
		for (final Object[] relationHandler : relationHandlersWithCorrectQualifiers)
		{
			//when
			final String relationQualifier = TestOneToManyHandler.getRelationQualifier((OneToManyHandler<?>) relationHandler[0]);

			//then
			assertThat(relationQualifier).isEqualTo(relationHandler[1]);
		}
	}

	private Object[][] getRelationHandlersWithCorrectQualifiers()
	{
		return Stream.concat(Arrays.stream(TestCatalogManager.getRelationHandlersWithCorrectQualifiers()),
				Arrays.stream(TestCommentsManager.getRelationHandlersWithCorrectQualifiers())).toArray(Object[][]::new);
	}

	private static class TestOneToManyHandler extends OneToManyHandler
	{

		public TestOneToManyHandler(final String targetItemType, final boolean partOf, final String foreignKeyAttr,
		                            final String orderNumberAttr, final boolean reorderable, final boolean asc)
		{
			super(targetItemType, partOf, foreignKeyAttr, orderNumberAttr, reorderable, asc);
		}

		public static String getRelationQualifier(final OneToManyHandler<?> handler)
		{
			final Field relationsInfoField = ReflectionUtils.findField(OneToManyHandler.class, "relationsInfo");
			ReflectionUtils.makeAccessible(relationsInfoField);
			final Supplier<RelationsData> relationsInfo = (Supplier<RelationsData>) ReflectionUtils.getField(relationsInfoField,
					handler);
			return relationsInfo.get().getRelationQualifier();
		}
	}

	private static class TestCatalogManager extends CatalogManager
	{
		public static Object[][] getRelationHandlersWithCorrectQualifiers()
		{
			return new Object[][]{
					{ ABSTRACTORDERENTRY2ABSTRACTORDERENTRYPRODUCTINFORELATIONPRODUCTINFOSHANDLER, "productInfos" },
					{ PRODUCT2FEATURERELATIONFEATURESHANDLER, "features" },
					{ PRODUCT2VARIANTRELATIONVARIANTSHANDLER, "variants" },
					{ PRODUCTREFERENCERELATIONPRODUCTREFERENCESHANDLER, "productReferences" }
			};
		}
	}

	private static class TestCommentsManager extends CommentsManager
	{
		public static Object[][] getRelationHandlersWithCorrectQualifiers()
		{
			return new Object[][]{
					{ ABSTRACTCOMMENTAUTHORRELATIONCREATEDCOMMENTSHANDLER, "createdComments" },
					{ COMMENTUSERSETTINGUSERRELATIONCOMMENTUSERSETTINGSHANDLER, "commentUserSettings" }
			};
		}
	}
}