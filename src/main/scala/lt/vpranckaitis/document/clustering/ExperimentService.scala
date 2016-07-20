package lt.vpranckaitis.document.clustering

import lt.vpranckaitis.document.clustering.FeatureSelection.FeatureVectors
import lt.vpranckaitis.document.clustering.clusterers.Clusterer
import lt.vpranckaitis.document.clustering.clusterers.kmeans.ClusteringResults
import lt.vpranckaitis.document.clustering.dto.ExtendedJsonProtocol._
import lt.vpranckaitis.document.clustering.dto._
import lt.vpranckaitis.document.clustering.storage.Storage
import lt.vpranckaitis.document.clustering.storage.schema.{Article, Experiment}
import org.joda.time.DateTime
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ExperimentService(storage: Storage) {

  lazy val consoleOutput = new ConsoleOutputExperimentService(storage)

  protected def getExperimentSummary(dataSet: Int, featureVectors: FeatureVectors, clusterer: Clusterer,
                               clusteringResults: ClusteringResults, runtime: Long) = {
    val dimensionality = featureVectors.documents.head.getDimensionality

    println(s"Article count: ${featureVectors.documents.size}, dimensionality: $dimensionality")

    val configuration = Configuration(featureVectors.logMessage, clusterer.logMessage)
    val evaluation = ClusteringEvaluation.evaluate(clusteringResults.clusters)
    val comment = ""

    ExperimentSummary(
      dataSet,
      DateTime.now,
      runtime,
      configuration,
      evaluation,
      comment)
  }

  protected def experimentSummaryToExperiment(experimentSummary: ExperimentSummary): Experiment = {
    import experimentSummary._
    Experiment(datasetId, date, runtime, configuration.toJson.prettyPrint, evaluation.toJson.prettyPrint, comment)
  }

  protected def saveExperiment(clusteringResults: ClusteringResults, experiment: ExperimentSummary): Future[Unit] = {
    storage.save(experimentSummaryToExperiment(experiment)) map { _ => () }
  }

  def runExperiment(dataSet: Int)(f: Seq[Article] => (FeatureVectors, Clusterer)): Future[Unit] = {
    storage.getArticlesByDataset(dataSet) flatMap { articles =>
      val (featureVectors, clusterer) = f(articles)

      val (clusteringResults, runtime) = Util.time { clusterer.clusterize(featureVectors.documents) }

      val experimentResults = getExperimentSummary(dataSet, featureVectors, clusterer, clusteringResults, runtime)
      println(experimentResults)
      saveExperiment(clusteringResults, experimentResults)
    }
  }

  class ConsoleOutputExperimentService private[ExperimentService] (storage: Storage) extends ExperimentService(storage) {
    override def saveExperiment(clusteringResults: ClusteringResults, experiment: ExperimentSummary): Future[Unit] = {
      println(experiment.toJson.prettyPrint)
      Future[Unit]()
    }
  }
}
