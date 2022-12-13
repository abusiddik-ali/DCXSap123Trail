package de.hybris.platform.jalo.flexiblesearch;

import de.hybris.platform.core.model.user.TitleModel;
import de.hybris.platform.jdbcwrapper.JdbcTestSupport;
import de.hybris.platform.servicelayer.ServicelayerBaseTest;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import de.hybris.platform.tx.Transaction;
import org.junit.Test;

import javax.annotation.Resource;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;

public class FlexibleSearchCacheUnitTransactionConsistencyTest extends ServicelayerBaseTest {

    public static final String SELECT = "SELECT  item_t0.PK  FROM junit_titles item_t0 WHERE ( item_t0.p_code ='";

    @Resource
    private FlexibleSearchService flexibleSearchService;

    @Resource
    private ModelService modelService;

    private final JdbcTestSupport.JdbcStatistics stats = JdbcTestSupport.createNewJdbcStatistics();

   @Test
    public void testDependentTypeGenerationsTheSameBeforeAndInTx(){
        final TitleModel title = modelService.create(TitleModel.class);
        String code = UUID.randomUUID().toString();
        title.setCode(code);
        modelService.save(title);

        stats.attachToCurrentThread();

        final SearchResult<TitleModel> search = search(code);
        assertThat(search.getCount()).isEqualTo(1);
        assertThat(search.getResult().get(0).getCode()).isEqualTo(code);
	    stats.assertThat().selectStatements().filteredOn(statement->statementPredicate(statement)).hasSize(1);

        Transaction.current().begin();

        final SearchResult<TitleModel> searchInTx = search(code);
        assertThat(searchInTx.getCount()).isEqualTo(1);
        assertThat(searchInTx.getResult().get(0).getCode()).isEqualTo(code);
        Transaction.current().commit();
	    stats.assertThat().selectStatements().filteredOn(statement->statementPredicate(statement)).hasSize(1);

        stats.detach();
    }

    private boolean statementPredicate(final String statement)
    {
   	    return statement.contains("titles") && statement.contains("SELECT") && statement.contains("p_code");
    }

    private SearchResult<TitleModel> search(String code) {
        Map<String, Object> params = new HashMap<>();
        params.put("code", code);
        SearchResult<TitleModel> search = flexibleSearchService.search("Select {pk} from {Title} where {code}=?code",params);
        return search;
    }
}
