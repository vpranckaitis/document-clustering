package lt.vpranckaitis.document.clustering

import lt.vpranckaitis.document.clustering.dto.ExtendedJsonProtocol._
import lt.vpranckaitis.document.clustering.dto._
import lt.vpranckaitis.document.clustering.storage.Storage
import spray.json._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Output extends App {
  val storage = new Storage()

  val experiments = storage.getExperiments() map { experiments =>
    for {
      e <- experiments
    } yield ExperimentSummary(
      e.datasetId,
      e.date,
      e.runtime,
      e.configuration.parseJson.convertTo[Configuration],
      e.evaluation.parseJson.convertTo[Evaluation],
      e.comment)
  }

  println(Await.result(experiments, Duration.Inf).toJson)
}
