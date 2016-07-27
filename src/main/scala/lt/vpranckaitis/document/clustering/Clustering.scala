package lt.vpranckaitis.document.clustering

import lt.vpranckaitis.document.clustering.ExperimentService.Experiment
import lt.vpranckaitis.document.clustering.clusterers.DistanceFunction
import lt.vpranckaitis.document.clustering.clusterers.kmeans.{ClassicKMeans, InitialMeans}
import lt.vpranckaitis.document.clustering.storage.Storage
import lt.vpranckaitis.document.clustering.storage.schema.Article

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Clustering extends App {

  val experimentService = new ExperimentService(new Storage)

  val experiments: Seq[Experiment] = for {
    seed <- Seq(45, 72468, 92438, 242, 85436)
  } yield { articles: Seq[Article] =>
    val featureVectors =
      FeatureSelection(articles).
        takeText().
        split().
        toLowercase().
        lengthAtLeast(1).
        stem().
        termFrequencyInverseDocumentFrequency().
        normalize().
        toFeatureVectors()

    val clusterer = new ClassicKMeans(10, DistanceFunction.Cosine, InitialMeans.Random(seed))

    (featureVectors, clusterer, "different seed experiment")
  }

  //---------------------------

  println(s"Experiment count: ${experiments.size}")

  val experimentsRun = experimentService.consoleOutput.runExperiments(3)(experiments)
  val successfulExperimentsCount = Await.result(experimentsRun, Duration.Inf)

  println(s"Successful experiments: ${successfulExperimentsCount}/${experiments.size}")
}
