package lt.vpranckaitis.document.clustering.storage

import lt.vpranckaitis.document.clustering.storage.schema._
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future

class Storage {
  val db = Database.forConfig("document-clustering.database")

  def save(o: Storable) = {
    val action = o match  {
      case a: Article => articlesQuery += a
      case e: Experiment => experimentsQuery returning experimentsQuery.map(_.id) += e
      case ca: ClusterArticle => clustersArticlesQuery += ca
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

  def getArticleById(id: Int): Future[Option[Article]] = {
    val q = for {
      a <- articlesQuery
      if a.id === id
    } yield a
    db.run(q.result.headOption)
  }

  def getExperiments(): Future[Seq[Experiment]] = {
    val q = for {
      e <- experimentsQuery
    } yield e

    db.run(q.result)
  }

  def getExperimentsByDataset(datasetId: Int): Future[Seq[Experiment]] = {
    val q = for {
      e <- experimentsQuery
      if e.datasetId === datasetId
    } yield e

    db.run(q.result)
  }

  def getExperimentsByCategory(source: String, category: String, subcategory: String, limit: Int = 100): Future[Seq[Article]] = {
    val q = for {
      a <- articlesQuery
      if (a.source === source && a.category === category && a.subcategory === subcategory)
    } yield a
    db.run(q.take(limit).result)
  }

  def getExperimentById(id: Int): Future[Option[Experiment]] = {
    val q = for {
      e <- experimentsQuery
      if e.id === id
    } yield e

    db.run(q.result.headOption)
  }

  def getClustersByExperimentId(id: Int): Future[Seq[(ClusterArticle, Article)]] = {
    val q = for {
      ca <- clustersArticlesQuery
      a <- articlesQuery
      if ca.experimentId === id && ca.articleId === a.id
    } yield (ca, a)

    db.run(q.result)
  }

  def update(o: Article) = {
    val q = for {
      a <- articlesQuery
      if (a.source === o.source && a.url === o.url)
    } yield a

    db.run(q.update(o))
  }

  def close(): Unit = db.close()
}
