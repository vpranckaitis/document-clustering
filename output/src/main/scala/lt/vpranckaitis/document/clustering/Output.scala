package lt.vpranckaitis.document.clustering

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import lt.vpranckaitis.document.clustering.dto.ExtendedJsonProtocol._
import lt.vpranckaitis.document.clustering.dto._
import lt.vpranckaitis.document.clustering.storage.Storage

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Output extends App {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  val storage = new Storage()
  val service = new Service(storage)

  val route =
    rejectEmptyResponse {
      pathPrefix("experiments") {
        pathEndOrSingleSlash {
          complete(service.getExperiments())
        } ~
        path(IntNumber) { id =>
          complete(service.getExperiment(id))
        } ~
        path(IntNumber / "clusters") { experimentId =>
          complete(service.getClustersByExperimentId(experimentId))
        } ~
        path(IntNumber / "cluster-sizes") { experimentId =>
          complete(service.getClusterSizesByExperimentId(experimentId))
        } ~
        path (IntNumber / "common-words") { experimentId =>
          parameters('unstem ? false) { unstem =>
            complete(service.getCommonWords(experimentId, unstem))
          }
        }
      }
    }

  Http().bindAndHandle(route, "localhost", 8000)
}
