package lt.vpranckaitis.document.clustering

import lt.vpranckaitis.document.clustering.ExperimentService.Experiment
import lt.vpranckaitis.document.clustering.clusterers.hierarchical.{ExtractionMethod, Hierarchical, LinkageMethod}
import lt.vpranckaitis.document.clustering.clusterers.{DistanceFunction, random}
import lt.vpranckaitis.document.clustering.clusterers.kmeans.{ClassicKMeans, InitialMeans}
import lt.vpranckaitis.document.clustering.clusterers.random.RandomClustering
import lt.vpranckaitis.document.clustering.storage.Storage
import lt.vpranckaitis.document.clustering.storage.schema.Article

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Clustering extends App {

  val experimentService = new ExperimentService(new Storage)

  val experiments: Seq[Experiment] = for {
    k <- 9 to 9
    seed <- Seq(45, 72468, 92438, 242, 85436)
    initialMeans <- Seq(InitialMeans.Random(seed), InitialMeans.FarthestPoints(seed), InitialMeans.KMeansPlusPlus(seed))
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

    //val clusterer = new Hierarchical(DistanceFunction.Cosine, LinkageMethod.Complete, ExtractionMethod.HDBSCANTargetK(k))
    val clusterer = new ClassicKMeans(k, DistanceFunction.Cosine, initialMeans)
    //val clusterer = new RandomClustering(k, seed)

    (featureVectors, clusterer, "Category mapped, k-means")
  }

  //---------------------------

  println(s"Experiment count: ${experiments.size}")

  val experimentsRun = experimentService.runExperiments(3)(experiments)
  val successfulExperimentsCount = Await.result(experimentsRun, Duration.Inf)

  println(s"Successful experiments: ${successfulExperimentsCount}/${experiments.size}")
}
