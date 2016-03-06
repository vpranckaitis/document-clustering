package lt.vpranckaitis.documentclustering.storage

import lt.vpranckaitis.documentclustering.storage.schema._
import slick.driver.PostgresDriver.api._

class Storage {
  val db = Database.forConfig("document-clustering.database")

  def save(o: Storable) = {
    val action = o match  {
      case a: Article => articlesQuery += a
    }

    db.run(action)
  }

  def streamArticles() = db.run(articlesQuery.take(1000).result.transactionally.withStatementParameters(fetchSize = 1000))

  def getArticlesByDataset(datasetId: Int) = {
    val q = for {
      a <- articlesQuery
      ad <- articlesDatasetsQuery
      if (ad.datasetId === datasetId && a.id === ad.articleId)
    } yield a
    db.run(q.result)
  }
}
