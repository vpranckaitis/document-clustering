package lt.vpranckaitis.document.clustering

import com.typesafe.scalalogging.StrictLogging
import lt.vpranckaitis.document.clustering.FeatureSelection.FeatureVectors
import lt.vpranckaitis.document.clustering.clusterers.Clusterer
import lt.vpranckaitis.document.clustering.clusterers.kmeans.ClusteringResults
import lt.vpranckaitis.document.clustering.dto.ExtendedJsonProtocol._
import lt.vpranckaitis.document.clustering.dto._
import lt.vpranckaitis.document.clustering.storage.Storage
import lt.vpranckaitis.document.clustering.storage.schema.{Article, ClusterArticle, Experiment}
import org.joda.time.DateTime
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ExperimentService {
  type Experiment = (Seq[Article]) => (FeatureSelection.FeatureVectors, Clusterer, String)
}

class ExperimentService(storage: Storage) extends StrictLogging {

  lazy val consoleOutput = new ConsoleOutputExperimentService(storage)

  protected def getExperimentSummary(dataSet: Int, featureVectors: FeatureVectors, clusterer: Clusterer,
                               clusteringResults: ClusteringResults, runtime: Long, comment: String) = {
    val dimensionality = featureVectors.documents.head.getDimensionality

    println(s"Article count: ${featureVectors.documents.size}, dimensionality: $dimensionality")

    val configuration = Configuration(featureVectors.logMessage, clusterer.logMessage)
    val evaluation = ClusteringEvaluation.evaluate(clusteringResults.clusters)

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
    Experiment(datasetId, date, runtime, configuration.toJson.toString, evaluation.toJson.toString, comment)
  }

  protected def saveClusters(clusteringResults: ClusteringResults, experimentId: Int): Future[Int] = {
    val clusterArticles = for {
      (documentsInCluster, cluster) <- clusteringResults.clusters.zipWithIndex
      documentInCluster <- documentsInCluster.unzip._1
      articleId <- documentInCluster.article.id
    } yield ClusterArticle(experimentId, cluster, articleId)

    clusterArticles.foldLeft(Future(0)){ (acc, ca) =>
      for {
        count <- acc
        insertedLines <- storage.save(ca) recover { case _ => 0 }
      } yield count + insertedLines
    }
  }

  protected def saveExperiment(clusteringResults: ClusteringResults, experiment: ExperimentSummary): Future[Unit] = {
    for {
      experimentId <- storage.save(experimentSummaryToExperiment(experiment))
      _ <- saveClusters(clusteringResults, experimentId)
    } yield ()
  }

  def runExperiment(dataSet: Int)(f: ExperimentService.Experiment): Future[Unit] = {
    storage.getArticlesByDataset(dataSet) flatMap { articles =>
      val (featureVectors, clusterer, comment) = f(articles)

      val (clusteringResults, runtime) = Util.time { clusterer.clusterize(featureVectors.documents) }

      val experimentResults = getExperimentSummary(dataSet, featureVectors, clusterer, clusteringResults, runtime, comment)
      println(experimentResults)
      saveExperiment(clusteringResults, experimentResults)
    }
  }

  def runExperiments(dataSets: Int*)(fs: Seq[ExperimentService.Experiment]): Future[Int] = {
    val dataSetsWithFunctions = for {
      dataSet <- dataSets
      experimentFunction <- fs
    } yield (dataSet, experimentFunction)

    dataSetsWithFunctions.foldLeft(Future(0)) { (acc, dataSetWithFunction) =>
      for {
        count <- acc
        (dataSet, f) = dataSetWithFunction
        newCount <- runExperiment(dataSet)(f) map { _ => count + 1 } recover {
          case ex =>
            logger.error("Failed experiment", ex)
            count
        }
      } yield newCount
    }
  }

  class ConsoleOutputExperimentService private[ExperimentService] (storage: Storage) extends ExperimentService(storage) {
    override def saveExperiment(clusteringResults: ClusteringResults, experiment: ExperimentSummary): Future[Unit] = {
      println(experiment.toJson.prettyPrint)
      Future[Unit]()
    }
  }
}
