package lt.vpranckaitis.document.clustering.storage.schema

import slick.driver.PostgresDriver.api._
import slick.lifted.{TableQuery, Tag}

case class ClusterArticle(experimentId: Int, cluster: Int, articleId: Int) extends Storable

class ClustersArticles(tag: Tag) extends Table[ClusterArticle](tag, "clusters_articles_xref") {
  def experimentId = column[Int]("id_experiment")

  def cluster = column[Int]("cluster")

  def articleId = column[Int]("id_article")

  def * = (experimentId, cluster, articleId) <> (ClusterArticle.tupled, ClusterArticle.unapply)
}

object clustersArticlesQuery extends TableQuery(new ClustersArticles(_)) {

}