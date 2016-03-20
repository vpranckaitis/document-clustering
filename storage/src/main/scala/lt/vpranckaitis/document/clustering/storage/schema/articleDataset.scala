package lt.vpranckaitis.document.clustering.storage.schema

import slick.driver.PostgresDriver.api._
import slick.lifted.{TableQuery, Tag}

case class ArticleDataset(articleId: Int, datasetId: Int) extends Storable

class ArticlesDatasets(tag: Tag) extends Table[ArticleDataset](tag, "articles_datasets_xref") {
  def articleId = column[Int]("id_article")

  def datasetId = column[Int]("id_dataset")

  def * = (articleId, datasetId) <> (ArticleDataset.tupled, ArticleDataset.unapply)
}

object articlesDatasetsQuery extends TableQuery(new ArticlesDatasets(_)) {

}