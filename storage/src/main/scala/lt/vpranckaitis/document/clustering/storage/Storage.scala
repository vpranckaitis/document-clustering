package lt.vpranckaitis.document.clustering.storage

import lt.vpranckaitis.document.clustering.storage.schema._
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future

class Storage {
  val db = Database.forConfig("document-clustering.database")

  def save(o: Storable) = {
    val action = o match  {
      case a: Article => articlesQuery += a
      case e: Experiment => experimentsQuery += e
    }

    db.run(action)
  }

  def streamArticles() = db.run(articlesQuery.take(1000).result.transactionally.withStatementParameters(fetchSize = 1000))

  def getArticlesByDataset(datasetId: Int): Future[Seq[Article]] = {
    val q = for {
      a <- articlesQuery
      ad <- articlesDatasetsQuery
      if (ad.datasetId === datasetId && a.id === ad.articleId)
    } yield a
    db.run(q.result)
  }

  def getExperiments(): Future[Seq[Experiment]] = {
    val q = for {
      e <- experimentsQuery
    } yield e

    db.run(q.result)
  }
}
