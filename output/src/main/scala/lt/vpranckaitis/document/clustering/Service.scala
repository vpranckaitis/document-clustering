package lt.vpranckaitis.document.clustering

import lt.vpranckaitis.document.clustering.dto.ExtendedJsonProtocol._
import lt.vpranckaitis.document.clustering.dto._
import lt.vpranckaitis.document.clustering.storage.Storage
import lt.vpranckaitis.document.clustering.storage.schema.Experiment
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Service(storage: Storage) {
  private def toExperimentSummary(e: Experiment) = {
    ExperimentSummary(
      e.id,
      e.datasetId,
      e.date,
      e.runtime,
      e.configuration.parseJson.convertTo[Configuration],
      e.evaluation.parseJson.convertTo[Evaluation],
      e.comment)
  }

  def getExperiments(): Future[Seq[ExperimentSummary]] = {
    storage.getExperimentsByDataset(5) map { experiments =>
      for {
        e <- experiments
      } yield toExperimentSummary(e)
    }
  }

  def getExperiment(id: Int): Future[Option[ExperimentSummary]] = {
    storage.getExperimentById(id) map { _ map { toExperimentSummary } }
  }

  def getClustersByExperimentId(experimentId: Int): Future[Seq[Cluster]] = {
    storage.getClustersByExperimentId(experimentId) map { clusters =>
      val grouped = clusters groupBy { _._1.cluster } mapValues { clusters =>
        for {
          article <- clusters.unzip._2
        } yield ArticleSummary(article.title, article.description, article.url)
      }

      grouped.values.toSeq map { Cluster(_) }
    }
  }

  def getClusterSizesByExperimentId(experimentId: Int): Future[Seq[Int]] = {
    storage.getClustersByExperimentId(experimentId) map { clusters =>
      val grouped = clusters groupBy { _._1.cluster } mapValues { _.size }

      grouped.values.toSeq.sorted(Ordering[Int].reverse)
    }
  }
}
