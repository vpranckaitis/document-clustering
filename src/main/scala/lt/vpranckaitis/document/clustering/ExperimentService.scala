package lt.vpranckaitis.document.clustering

import lt.vpranckaitis.document.clustering.FeatureSelection.FeatureVectors
import lt.vpranckaitis.document.clustering.clusterers.Clusterer
import lt.vpranckaitis.document.clustering.clusterers.kmeans.ClusteringResults
import lt.vpranckaitis.document.clustering.dto.Configuration
import lt.vpranckaitis.document.clustering.dto.ExtendedJsonProtocol._
import lt.vpranckaitis.document.clustering.storage.Storage
import lt.vpranckaitis.document.clustering.storage.schema.{Article, Experiment}
import org.joda.time.DateTime
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ExperimentService(storage: Storage) {

  def saveExperiment(dataSet: Int, featureVectors: FeatureVectors, clusterer: Clusterer,
                     clusteringResults: ClusteringResults, runtime: Long): Future[Unit] = {
    val dimensionality = featureVectors.documents.head.getDimensionality

    println(s"Article count: ${featureVectors.documents.size}, dimensionality: $dimensionality")

    val configuration = Configuration(featureVectors.logMessage, clusterer.logMessage)
    val evaluation = ClusteringEvaluation.evaluate(clusteringResults.clusters)
    val comment = ""

    val experiment = Experiment(
      dataSet,
      DateTime.now,
      runtime,
      configuration.toJson.toString,
      evaluation.toJson.toString,
      comment)

    storage.save(experiment) map { _ => () }
  }

  def runExperiment(dataSet: Int)(f: Seq[Article] => (FeatureVectors, Clusterer)): Future[Unit] = {
    storage.getArticlesByDataset(dataSet) flatMap { articles =>
      val (featureVectors, clusterer) = f(articles)

      val (results, runtime) = Util.time { clusterer.clusterize(featureVectors.documents) }

      saveExperiment(dataSet, featureVectors, clusterer, results, runtime)
    }
  }

}
